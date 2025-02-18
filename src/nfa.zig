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
