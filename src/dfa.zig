const std = @import("std");
const nfa = @import("nfa.zig");
const parser = @import("parser.zig");

/// A state in a dfa
pub const State = struct {
    /// Null transitions represent transitions that should raise errors
    trans: [256]?usize = [1]?usize{null} ** 256,

    /// non-null token represents accepting state
    token: ?usize = null,
};

/// When converting an nfa to a dfa, you use a set to track each of the new states
const Set = struct {
    nfa_states: []const usize,
    dfa_state: State,

    /// Find neighbors of the state and construct a set from that
    fn init(fa: []const nfa.State, from: []const usize, on: ?u8) Set {
        // Visit every state's neighbors from the list
        var n = nfa.Neighbors.init(fa, on);
        for (from) |state| {
            n.visit(state, false);
        }
        const states = n.neighbors();

        // Return the first seen final token, (they should all be the same)
        // TODO: (add sanity check maybe?)
        return for (states) |state| {
            if (fa[state].token) |token| {
                break .{
                    .nfa_states = states,
                    .dfa_state = .{ .token = token },
                };
            }
        } else .{ .nfa_states = states, .dfa_state = .{} };
    }
};

/// Runs the conversion and minimization code to give you a complete dfa
pub fn convert(fa: []const nfa.State) []const State {
    const dfa = nfa_to_dfa(fa);
    return minimize(dfa);
}

/// Returns a transition table with the first entry corresponding to the starting state
fn nfa_to_dfa(fa: []const nfa.State) []const State {
    // Create starting state
    var dfa = std.BoundedArray(Set, 1024).init(0) catch unreachable;
    dfa.appendAssumeCapacity(Set.init(fa, &.{0}, null));

    // A lot of logic happens here so set the branch eval quota accordingly
    @setEvalBranchQuota(fa.len * fa.len * 1000 * 1000);

    // Start finding neighbors as long as there are more
    var current: usize = 0;
    while (current < dfa.len) : (current += 1) {
        // Find neighbors for every transition
        const state = &dfa.buffer[current];
        for (0..256) |byte| {
            const set = Set.init(fa, state.*.nfa_states, byte);

            // This represents no transitions, so don't add the transition (or an empty set)
            if (set.nfa_states.len == 0) continue;

            // Set the transition to what the set's dfa index is
            state.*.dfa_state.trans[byte] = put_set: {
                // Try to find the state and return it's index
                for (dfa.slice(), 0..) |i, idx| {
                    if (std.mem.eql(usize, i.nfa_states, set.nfa_states)) {
                        break :put_set idx;
                    }
                }

                // Add a new state to do
                dfa.append(set) catch @panic("dfa conversion set overflow!");
                break :put_set dfa.len - 1;
            };
        }
    }

    // Convert the sets to an actual dfa
    var dfa_states: [dfa.len]State = undefined;
    for (dfa.slice(), &dfa_states) |set, *state| {
        state.* = set.dfa_state;
    }
    const final_dfa = dfa_states;
    return &final_dfa;
}

/// Returns a bitset that can carry a set or unset for every state in the dfa
fn BitSet(comptime dfa: []const State) type {
    return std.StaticBitSet(dfa.len);
}

/// Gets the final states from a dfa as a set
fn finalStates(comptime dfa: []const State) []const BitSet(dfa) {
    // First get the number of unique final states
    var unique = std.BoundedArray(usize, dfa.len).init(0) catch unreachable;
    for (dfa) |state| {
        const tok = state.token orelse continue;
        if (std.mem.indexOfScalar(usize, unique.slice(), tok)) |_| continue;
        unique.appendAssumeCapacity(tok);
    }

    var sets = std.BoundedArray(BitSet(dfa), unique.len).init(0) catch unreachable;
    for (unique.slice()) |tok| {
        var set = BitSet(dfa).initEmpty();
        for (dfa, 0..) |d, i| {
            if (d.token == tok) set.set(i);
        }
        sets.appendAssumeCapacity(set);
    }

    const final = sets.slice()[0..].*;
    return &final;
}

/// This will get every state that transitions to any state in `s` on transition `c`
fn getTrans(comptime dfa: []const State, s: BitSet(dfa), c: u8) BitSet(dfa) {
    // Use a bit set to automatically not do repeated counts
    var is_trans = std.StaticBitSet(dfa.len).initEmpty();
    var iter = s.iterator(.{});
    while (iter.next()) |state| {
        for (dfa, 0..) |d, i| {
            if (d.trans[c] == state) is_trans.set(i);
        }
    }
    return is_trans;
}

/// Add a state to the dfa from the minimized state set
fn visitStateSet(
    comptime dfa: []const State,
    comptime p: []const BitSet(dfa),
    comptime states: *std.BoundedArray(State, p.len),
    comptime visited: []?usize,
    state: usize,
) usize {
    const set = p[state];

    // Go through every state in set to get information about the new dfa state
    const dfa_state = states.addOneAssumeCapacity();
    dfa_state.* = .{};
    visited[state] = states.len - 1;
    var iter = set.iterator(.{});
    while (iter.next()) |s| {
        // If we come across a final state in the set, add it to the set
        if (dfa[s].token) |t| dfa_state.*.token = t;

        // It's harder to iterate over the bits so we go over every transition for this state
        // here. Supposedly everything should work out fine, so no error checking here, just
        // overwrite anything that is already there I guess.
        for (0..256) |c| {
            // Find where this transition would goto in the new dfa
            const to = dfa[s].trans[c] orelse continue;
            dfa_state.*.trans[c] = for (p, 0..) |j, k| {
                if (j.isSet(to)) {
                    if (visited[k]) |idx| {
                        break idx;
                    } else {
                        break visitStateSet(dfa, p, states, visited, k);
                    }
                }
            } else unreachable;
        }
    }

    // Add the state
    return visited[state].?;
}

/// There won't be any unreachable states, so just remove duplicate states
/// This uses hopcroft's algorithm, which tbh I am not quite sure how it works
fn minimize(dfa: []const State) []const State {
    // Since each state should be a seperate partition, we get an array of final state partitions
    const f = finalStates(dfa);
    const f_all = blk: {
        var all = BitSet(dfa).initEmpty();
        for (f) |set| {
            all.setUnion(set);
        }
        break :blk all;
    };
    
    // All states that are not final states
    const not_final = BitSet(dfa).initFull().differenceWith(f_all);

    // Partitions?
    var p = std.BoundedArray(BitSet(dfa), dfa.len + 1).init(0) catch unreachable;
    for (f) |set| {
        p.appendAssumeCapacity(set);
    }
    p.appendAssumeCapacity(not_final);

    // Words?
    var w = std.BoundedArray(BitSet(dfa), dfa.len + 1).init(0) catch unreachable;
    for (f) |set| {
        w.appendAssumeCapacity(set);
    }
    w.appendAssumeCapacity(not_final);

    while (w.len > 0) {
        const a = w.orderedRemove(0);
        for (0..256) |input| {
            var i = 0;
            while (i < p.len) : (i += 1) {
                const x = getTrans(dfa, a, input);
                const y = p.get(i);
                const intr = x.intersectWith(y);
                const diff = y.differenceWith(x);
                const intr_cnt = intr.count();
                const diff_cnt = diff.count();

                if (intr_cnt == 0 or diff_cnt == 0) continue;
                p.set(i, diff);
                p.insert(i, intr) catch @panic("dfa minimization overflow!");
                i += 1;

                for (w.slice(), 0..) |j, k| {
                    if (!j.eql(y)) {
                        w.set(k, diff);
                        w.insert(k, intr) catch @panic("dfa minimization overflow!");
                        break;
                    }
                } else {
                    if (intr_cnt <= diff_cnt) {
                        w.append(intr) catch @panic("dfa minimization overflow!");
                    } else {
                        w.append(diff) catch @panic("dfa minimization overflow!");
                    }
                }
            }
        }
    }

    // `p` now contains a list of bitsets, where every bit set in each signifies that the new state
    // at that index contains the state at that index as well
    var states = std.BoundedArray(State, p.len).init(0) catch unreachable;

    // Doubles up as something to show if a state set has been visited, and to map old state set
    // indexes to new state sets
    var visited = [1]?usize{null} ** p.len;

    // Visit the initial state to make it the first index, and then visit everything after it
    _ = visitStateSet(dfa, p.slice(), &states, &visited, for (p.slice(), 0..) |s, i| {
        if (s.isSet(0)) break i;
    } else unreachable);

    const final = states.slice()[0..].*;
    return &final;
}

/// Helper function to generate states
fn testState(trans: []const struct { u8, usize }, token: ?usize) State {
    var state = State{ .token = token };
    inline for (trans) |t| {
        state.trans[t[0]] = t[1];
    }
    return state;
}

test "nfa to dfa conversion" {
    try std.testing.expectEqualSlices(State, comptime &.{
        testState(&.{.{ 'a', 1 }}, null),
        testState(&.{.{ 'b', 2 }}, null),
        testState(&.{.{ 'c', 3 }}, null),
        testState(&.{}, 0),
    }, comptime convert(nfa.construct(parser.parse("abc").ok, 0)));
    try std.testing.expectEqualSlices(State, comptime &.{
        testState(&.{.{ 'a', 1 }}, null),
        testState(&.{.{ 'a', 1 }}, 0),
    }, comptime convert(nfa.construct(parser.parse("a+").ok, 0)));
    try std.testing.expectEqualSlices(State, comptime &.{
        testState(&.{ .{ 'a', 1 }, .{ 'b', 1 } }, null),
        testState(&.{}, 0),
    }, comptime convert(nfa.construct(parser.parse("a|b").ok, 0)));
    try std.testing.expectEqualSlices(State, comptime &.{
        testState(&.{.{ 'a', 0 }}, 0),
    }, comptime convert(nfa.construct(parser.parse("a*").ok, 0)));
}
