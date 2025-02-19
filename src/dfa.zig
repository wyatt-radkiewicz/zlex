const std = @import("std");
const nfa = @import("nfa.zig");
const parser = @import("parser.zig");

/// A state in a dfa
pub const State = struct {
    /// Null transitions represent transitions that should raise errors
    trans: [256]?usize = [1]?usize{null} ** 256,

    /// non-null token represents accepting state
    token: ?usize = null,

    pub fn format(
        self: State,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("State{{ .trans = {{ ", .{});
        for (self.trans, 0..) |t, i| {
            if (t) |trans| {
                try writer.print("'{c}' -> {}", .{ @as(u8, @truncate(i)), trans });
            }
        }
        try writer.print(" }}, token = {?} }} }}", .{self.token});
    }
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

    pub fn format(
        self: Set,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("Set{{ nfa_states = {any}, dfa_state = State{{ trans = .{{ ", .{self.nfa_states});
        for (self.dfa_state.trans, 0..) |t, i| {
            if (t) |trans| {
                try writer.print("'{c}' -> {}", .{ i, trans });
            }
        }
        try writer.print("}}, token = {?} }} }}", .{self.dfa_state.token});
    }
};

/// Returns a transition table with the first entry corresponding to the starting state
pub fn convert(fa: []const nfa.State) []const State {
    // Create starting state
    var dfa = std.BoundedArray(Set, 1024).init(0) catch unreachable;
    dfa.append(Set.init(fa, &.{0}, null)) catch unreachable;

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
}
