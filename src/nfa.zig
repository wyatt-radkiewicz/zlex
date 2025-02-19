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
    const start_state = nfa.get(start);
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
        var bitset = std.StaticBitSet(256).initEmpty();
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

/// Helper struct used in the finding of neighbor states in an nfa
pub const Neighbors = struct {
    /// These are nodes that we should add as neighbors
    seen: std.BoundedArray(usize, 256),

    /// What NFA we are getting neighbors for
    nfa: []const State,

    /// What transitions we are adding states on
    on: ?u8,

    /// Create an empty neighbors finder
    pub fn init(nfa: []const State, on: ?u8) Neighbors {
        return .{
            .seen = std.BoundedArray(usize, 256).init(0) catch unreachable,
            .nfa = nfa,
            .on = on,
        };
    }

    /// Visit a node and add it to the seen list and possibly to the added list
    /// If not being called recursivly, then add_seen should start out as false, and
    /// state can be the state you wish to find neighbors for. Each call of visit will
    /// add more nodes to the list
    pub fn visit(self: *Neighbors, state: usize, add_seen: bool) void {
        // Add this state to the added list using a binary search/insert to preserve order
        dont_add: {
            if (add_seen or self.on == null) {
                for (self.seen.slice()) |seen| {
                    if (seen == state) break :dont_add;
                }
                
                var left = 0;
                var right = self.seen.len;
                while (right - left > 1) {
                    const mid = left + right / 2;
                    const val = self.seen.get(mid);

                    if (state > val) {
                        left = mid + 1;
                    } else if (state < val) {
                        right = mid;
                    } else {
                        // We already checked for the state
                        unreachable;
                    }
                }

                // Where to add, before or after?
                const idx = if (left == self.seen.len or state <= self.seen.get(left))
                    left
                else
                    right;

                // Insert the seen state
                self.seen.insert(idx, state) catch {
                    @panic("nfa neighbors overflow");
                };
            }
        }

        // Visit neighbors
        for (self.nfa[state].trans) |trans| {
            if (trans == null) continue;
            switch (trans.?.on) {
                .lambda => self.visit(trans.?.to, add_seen),
                .symbol => |bytes| {
                    // If we've already 'added seen' then don't go any more that 1 away
                    if (!add_seen and self.on != null and bytes.isSet(self.on.?)) {
                        self.visit(trans.?.to, true);
                    }
                },
            }
        }
    }

    /// Returns the neighbors found by the finder
    pub fn neighbors(self: Neighbors) []const usize {
        const final: [self.seen.len]usize = self.seen.slice()[0..].*;
        return &final;
    }
};

// Used in tests
fn testSingleBitset(idx: usize) Trans.Input {
    var bitset = std.StaticBitSet(256).initEmpty();
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
            .to = 2,
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
    }, comptime construct(parser.parse("a+").ok, 0));
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
            .on = testSingleBitset('a'),
        }, null }, .token = null },
        State{ .trans = .{ .{
            .to = 1,
            .on = testSingleBitset('b'),
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
    try std.testing.expectEqualSlices(usize, &.{ 0, 2, 3 }, comptime blk: {
        const nfa = construct(parser.parse("a|b").ok, 0);
        var n = Neighbors.init(nfa, null);
        n.visit(0, false);
        break :blk n.neighbors();
    });
    try std.testing.expectEqualSlices(usize, &.{ 1, 2, 3 }, comptime blk: {
        const nfa = construct(parser.parse("a*").ok, 0);
        var n = Neighbors.init(nfa, 'a');
        n.visit(0, false);
        break :blk n.neighbors();
    });

    const nfa = comptime construct(parser.parse("a+").ok, 0);
    try std.testing.expectEqualSlices(usize, &.{0}, comptime blk: {
        var n = Neighbors.init(nfa, null);
        n.visit(0, false);
        break :blk n.neighbors();
    });
    try std.testing.expectEqualSlices(usize, &.{ 0, 1, 2 }, comptime blk: {
        var n = Neighbors.init(nfa, 'a');
        n.visit(0, false);
        break :blk n.neighbors();
    });
    try std.testing.expectEqualSlices(usize, &.{}, comptime blk: {
        var n = Neighbors.init(nfa, 'b');
        n.visit(0, false);
        break :blk n.neighbors();
    });
    try std.testing.expectEqualSlices(usize, &.{ 0, 1, 2 }, comptime blk: {
        var n = Neighbors.init(nfa, 'a');
        n.visit(2, false);
        break :blk n.neighbors();
    });
    try std.testing.expectEqualSlices(usize, &.{}, comptime blk: {
        var n = Neighbors.init(nfa, 'b');
        n.visit(2, false);
        break :blk n.neighbors();
    });
    try std.testing.expectEqualSlices(usize, &.{ 0, 1, 2 }, comptime blk: {
        var n = Neighbors.init(nfa, 'a');
        n.visit(0, false);
        n.visit(1, false);
        n.visit(2, false);
        break :blk n.neighbors();
    });
}
