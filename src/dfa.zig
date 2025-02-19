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

    /// Find lambda neighbors of the state and construct a set from that
    fn init(fa: []const nfa.State, from: usize, on: ?u8) Set {
        const neighbors = nfa.Neighbors.find(fa, from, on);
        return .{
            .nfa_states = neighbors.states,
            .dfa_state = .{ .token = neighbors.token },
        };
    }

    /// Create a new set from set edges, with a dfa_state only containing the common token.
    /// If there is no common token, then an `@compileError` will be thrown
    fn combine(sets: []const Set) Set {
        var full_len: usize = 0;
        var token: ?usize = null;
        for (sets) |set| {
            full_len += set.nfa_states.len;
            if (set.dfa_state.token) |t| {
                if (token == null or token == t) {
                    token = t;
                } else {
                    @compileError("joining together multiple sets in nfa->dfa conversion!");
                }
            }
        }

        const states = get_full_states: {
            // All the states ordered, without repeating entries
            var states = std.BoundedArray(usize, full_len).init(0) catch unreachable;

            // Keep adding the states until there are no more to add
            var completed_sets = 0;
            var idxs = [1]usize{0} ** sets.len;
            while (completed_sets != sets.len) {
                // Get the min state id
                var min: usize = std.math.maxInt(usize);
                for (sets, idxs) |set, idx| {
                    if (set.nfa_states[idx] < min) min = set.nfa_states[idx];
                }

                // Advance all states that have this state id (since they are ordered and don't have
                // repeating entries, we only need to check this once)
                completed_sets = 0;
                for (sets, &idxs) |set, *idx| {
                    if (set.nfa_states[idx.*] == min) idx.* += 1;
                }

                // Add the state to the full list
                states.append(min) catch unreachable;
            }

            break :get_full_states @as([states.len]usize, states.slice()[0..].*);
        };
        
        return .{
            .dfa_state = .{ .token = token },
            .nfa_states = states,
        };
    }
};

const Dfa = std.BoundedArray(Set, 1024);

/// Tries to add the set to the dfa, and if it's already there, returns the pointer of the current
/// one instead.
fn addSet(dfa: *Dfa, set: Set) usize {
    for (dfa.slice(), 0..) |i, idx| {
        if (std.mem.eql(usize, i.nfa_states, set.nfa_states)) return idx;
    }
    dfa.append(set) catch @panic("dfa conversion set overflow!");
    return dfa.len - 1;
}

/// Returns a transition table with the first entry corresponding to the starting state
pub fn convert(fa: []const nfa.State) []const State {
    // Create starting state
    var dfa = Dfa.init(0) catch unreachable;
    _ = addSet(dfa, Set.init(fa, 0));

    // Start finding neighbors as long as there are more
    var current: usize = 0;
    while (current < dfa.len) : (current += 1) {
        // Find neighbors for every transition
        const state = &dfa.buffer[current];
        for (0..256) |byte| {
            var sets: [state.*.nfa_states.len]Set = undefined;
            for (state.*.nfa_states, 0..) |nfa_state, idx| {
                sets[idx] = Set.init(fa, nfa_state, byte);
            }

            const combined = Set.combine(&sets);
            state.dfa_state.trans[byte] = addSet(combined);
        }
    }
    
    var dfa_states: [dfa.len]State = undefined;
    for (dfa.slice(), 0..) |set, i| {
        dfa_states[i] = set.dfa_state;
    }
    const final_dfa = dfa_states;
    return &final_dfa;
}

test "nfa to dfa conversion" {
}
