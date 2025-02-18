const std = @import("std");
const parser = @import("parser.zig");
const Range = @import("Range.zig");

pub const State = struct {
    /// Transitions leading to other states
    trans: [2]?Trans,

    /// States with non-null token values represent final states
    token: ?usize,
};

pub const Trans = struct {
    /// Which state the transition leads to
    to: usize,

    /// 256 transitions for every byte representation, or nothing in lambda transitions
    on: Input,

    /// When a transition is not a lambda transition, transition on this input
    pub const Input = union(enum) {
        lambda: void,
        symbol: std.StaticBitSet(256),
    };
};

/// In-construction nfa. Only used internally
/// We return the user an array of states instead of a tree with pointers because we need to be able
/// to represent circular refrences in the nfa
const Nfa = std.BoundedArray(State, 1024);

/// Adds a state to the nfa and returns its id
fn addState(nfa: *Nfa, state: State) usize {
    nfa.append(state) catch std.debug.panic("nfa state overflow ({} states)!", .{nfa.len + 1});
    return nfa.len - 1;
}

/// Manipulates the nfa so that the start state has the first index
fn normalizeNfa(nfa: *Nfa, start: usize) void {
    // Move the state to the start
    const start_state = nfa.buffer[start];
    std.mem.copyBackwards(State, nfa.buffer[1 .. start + 1], nfa.buffer[0..start]);
    nfa.buffer[0] = start_state;

    // Update refrences
    for (nfa.slice()) |*state| {
        inline for (&state.*.trans) |*trans| {
            if (trans.* == null) continue;
            const to = &trans.*.?.to;
            if (to.* < start) {
                to.* += 1;
            } else if (to.* == start) {
                to.* = 0;
            }
        }
    }
}

/// Creates the nfa from the ast.
/// ---
/// - `token` is the final state's token index
/// ---
/// returns:
/// - starting state
pub fn construct(ast: *const parser.Node, token: usize) []const State {
    var nfa = Nfa.init(0) catch unreachable;
    const final = addState(&nfa, .{
        .trans = .{ null, null },
        .token = token,
    });
    const start = fragment(&nfa, ast, final);
    normalizeNfa(&nfa, start);
    const final_nfa: [nfa.len]State = nfa.slice()[0..].*;
    return &final_nfa;
}

/// Creates the nfa from the ast.
/// ---
/// - `ast` the ast node to build this nfa state and transition from
/// - `next` is the state for this one to transition to
fn fragment(nfa: *Nfa, ast: *const parser.Node, next: usize) usize {
    return switch (ast.*) {
        .either => |node| addState(nfa, .{ .trans = .{ .{
            .to = fragment(nfa, node.left, next),
            .on = .lambda,
        }, .{
            .to = fragment(nfa, node.right, next),
            .on = .lambda,
        } }, .token = null }),
        .optional => |child| addState(nfa, .{ .trans = .{ .{
            .to = fragment(nfa, child, next),
            .on = .lambda,
        }, .{
            .to = next,
            .on = .lambda,
        } }, .token = null }),
        .one_many, .zero_many => |child| blk: {
            const end = addState(nfa, .{
                .trans = .{ null, .{ .to = next, .on = .lambda } },
                .token = null,
            });
            const start = fragment(nfa, child, end);
            nfa.buffer[end].trans[0] = .{ .to = start, .on = .lambda };

            break :blk switch (ast.*) {
                .zero_many => addState(nfa, .{
                    .trans = .{
                        .{ .to = start, .on = .lambda },
                        .{ .to = end, .on = .lambda },
                    },
                    .token = null,
                }),
                else => start,
            };
        },
        .sequence => |node| fragment(nfa, node.left, fragment(nfa, node.right, next)),
        .symbol => |range| rangeFragment(nfa, range, next),
    };
}

/// Create a part of an nfa from a range
pub fn rangeFragment(nfa: *Nfa, range: Range, next: usize) usize {
    if (range.isAscii()) {
        var bitset = std.StaticBitSet(256).initFull();
        for (range.start, range.end) |start, end| {
            for (start..end + 1) |i| {
                bitset.setValue(i, true);
            }
        }
        return addState(nfa, .{
            .trans = .{ .{
                .to = next,
                .on = .{ .symbol = bitset },
            }, null },
            .token = null,
        });
    } else {
        @compileError("TODO: implement unicode nfa transitions");
    }
}

/// Used in the process of finding neighbors of a state
const NeighborFinder = struct {
    array: std.BoundedArray(usize, 128),

    const Self = @This();

    pub fn init() Self {
        return .{ .array = std.BoundedArray(usize, 128).init(0) catch unreachable };
    }

    /// Returns `true` if the state wasn't there, or `false` if it was already there
    pub fn putState(self: *Self, state: usize) bool {
        // Binary search to get the position where it either is, or where we are going to add it
        var left: usize = 0;
        var right: usize = self.*.array.len;
        var idx = 0;
        while (right - left > 1) {
            idx = (left + right) / 2;
            const mid = self.*.array.buffer[idx];

            if (state > mid) {
                left = idx + 1;
            } else if (state < mid) {
                right = idx;
            } else {
                // Found a match, return false
                return false;
            }
        }

        idx = if (self.*.array.len > 0 and state > self.*.array.buffer[left]) right else left;
        self.*.array.insert(idx, state) catch @panic("nfa putState overflow!");
        return true;
    }

    /// Find the neighbors for this set starting at this state. If depth is greater than 0, then
    /// don't follow any non-lambda nodes
    pub fn find(
        self: *Self,
        nfa: []const State,
        state: usize,
        exclude: usize,
        on: ?u8,
        depth: usize,
    ) void {
        inline for (nfa[state].trans) |trans| {
            if (trans == null) continue;
            if (switch (trans.?.on) {
                .lambda => true,
                .symbol => |bitset| depth < 1 and on != null and bitset.isSet(on.?),
            } and trans.?.to != exclude and self.putState(trans.?.to)) {
                self.find(
                    nfa,
                    trans.?.to,
                    exclude,
                    on,
                    depth + @intFromBool(std.meta.activeTag(trans.?.on) == .lambda),
                );
            }
        }
    }
};

/// Find all direct neighbors of the specified state, while following lambda transitions
/// This returns a sorted array with no repeating elements
pub const Neighbors = struct {
    /// The states the state is connected to (not including itself)
    states: []const usize,

    /// Shows that one of the states is an accept state with this token
    token: ?usize,

    pub fn find(nfa: []const State, state: usize, on: ?u8) Neighbors {
        var neighbors = NeighborFinder.init();
        neighbors.find(nfa, state, state, on, 0);
        const final = neighbors.array.slice()[0..].*;
        const token = for (final) |i| {
            if (nfa[i].token) |token| break token;
        } else null;
        return .{ .states = &final, .token = token };
    }
};

fn testSingleBitset(idx: usize) Trans.Input {
    var bitset = std.StaticBitSet(256).initFull();
    bitset.setValue(idx, true);
    return .{ .symbol = bitset };
}

test "nfa construction" {
    try std.testing.expectEqualSlices(State, &.{
        State{ .trans = .{ .{
            .to = 3,
            .on = testSingleBitset('a'),
        }, null }, .token = null },
        State{ .trans = .{
            null,
            null,
        }, .token = 0 },
        State{ .trans = .{ .{
            .to = 1,
            .on = testSingleBitset('c'),
        }, null }, .token = null },
        State{ .trans = .{ .{
            .to = 2,
            .on = testSingleBitset('b'),
        }, null }, .token = null },
    }, comptime construct(parser.parse("abc").ok, 0));
    try std.testing.expectEqualSlices(State, &.{
        State{ .trans = .{ .{
            .to = 3,
            .on = testSingleBitset('a'),
        }, null }, .token = null },
        State{ .trans = .{
            null,
            null,
        }, .token = 0 },
        State{ .trans = .{ .{
            .to = 0,
            .on = .lambda,
        }, .{
            .to = 1,
            .on = .lambda,
        } }, .token = null },
        State{ .trans = .{ .{
            .to = 2,
            .on = testSingleBitset('b'),
        }, null }, .token = null },
    }, comptime construct(parser.parse("(ab)+").ok, 0));
    try std.testing.expectEqualSlices(State, &.{
        State{ .trans = .{ .{
            .to = 3,
            .on = .lambda,
        }, .{
            .to = 2,
            .on = .lambda,
        } }, .token = null },
        State{ .trans = .{
            null,
            null,
        }, .token = 0 },
        State{ .trans = .{ .{
            .to = 3,
            .on = .lambda,
        }, .{
            .to = 1,
            .on = .lambda,
        } }, .token = null },
        State{ .trans = .{ .{
            .to = 2,
            .on = testSingleBitset('a'),
        }, null }, .token = null },
    }, comptime construct(parser.parse("a*").ok, 0));
    try std.testing.expectEqualSlices(State, &.{
        State{ .trans = .{ .{
            .to = 2,
            .on = .lambda,
        }, .{
            .to = 3,
            .on = .lambda,
        } }, .token = null },
        State{ .trans = .{
            null,
            null,
        }, .token = 0 },
        State{ .trans = .{ .{
            .to = 1,
            .on = testSingleBitset('b'),
        }, null }, .token = null },
        State{ .trans = .{ .{
            .to = 1,
            .on = testSingleBitset('a'),
        }, null }, .token = null },
    }, comptime construct(parser.parse("a|b").ok, 0));
    try std.testing.expectEqualSlices(State, &.{
        State{ .trans = .{ .{
            .to = 2,
            .on = .lambda,
        }, .{
            .to = 1,
            .on = .lambda,
        } }, .token = null },
        State{ .trans = .{
            null,
            null,
        }, .token = 0 },
        State{ .trans = .{ .{
            .to = 1,
            .on = testSingleBitset('a'),
        }, null }, .token = null },
    }, comptime construct(parser.parse("a?").ok, 0));
}

test "nfa neighbors" {
    try std.testing.expectEqualDeep(
        Neighbors{ .states = &.{ 2, 3 }, .token = null },
        comptime Neighbors.find(construct(parser.parse("a|b").ok, 0), 0, null),
    );
    try std.testing.expectEqualDeep(
        Neighbors{ .states = &.{ 1, 2, 3 }, .token = 0 },
        comptime Neighbors.find(construct(parser.parse("a*").ok, 0), 0, 'a'),
    );
}
