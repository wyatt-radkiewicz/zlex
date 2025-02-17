const std = @import("std");

/// Generates a lexer iterator with a method that returns an union defining the matched token
/// ---
/// Parameters:
/// - `Space` defines a type to just put some state for the lexer actions. By default
/// the lexer already comes with some bear bones state. This includes:
///   - Line and column in the text
/// - `tokens` describes a tuple of matching patterns, each corresponding to a variant
/// in the token type returned by the lexer type.
/// ---
/// Example:
/// ```zig
/// Lexer(void, .{
///     .variant_with_void_type = "matching_pattern",
///     .plus_eq = "+=",
///     .number = struct {
///         pub const capture = "(0|([1-9][0-9]*))(.[0-9]+)?";
///
///         // You can omit this if you like, which will make this equivalent to the above patterns
///         // You can also choose not to return an error if you don't need one (less branching!)
///         pub fn action(state: *State, match: []const u8) !f64 {
///             return std.fmt.parseFloat(f64, match);
///         }
///     },
///     .whitespace = struct {
///         pub const skip = "[ \t\n]+";
///
///         // You can ommit this as well like above
///         pub fn action(state: *State, match: []const u8) void {
///             // Even though the whitespace should be skipped you can still see what matched here
///             _ = state;
///             _ = match;
///         }
///     },
/// }).init(state_object, src_string)
/// ```
pub fn Lexer(comptime State: type, comptime tokens: anytype) type {
    return struct {
        state: State,

        const Self = @This();
        pub const Token = TokenUnion(TokenTag(tokens), tokens);
    };
}


/// Describes a final state index and its corresponding token that it finishes
const Final = struct {
    /// What index in the states array this final state is
    idx: usize,

    /// A token identifier.
    tok: usize,
};

/// A nondeterministic finite automata
const Nfa = struct {
    /// Which is the starting state
    start: usize,

    /// What state is the ending state
    final: []const Final,

    /// The backing list of states
    states: []const State,

    /// Describes a transition (line) connecting this state to another one
    pub const Transition = union(enum) {
        /// This is a lambda transition
        lambda: usize,

        /// Describes what bytes this state transitions on. Null defined error transitions.
        symbol: [256]?usize,

        /// Temporary value used when parsing to represent unconnected nodes
        pub const unconnected = std.math.maxInt(usize);

        /// Returns a symbol state transition with all null states
        pub fn nullSymbols() Transition {
            return .{ .symbol = [1]?usize{null} ** 256 };
        }

        /// Sets all unconnected values in the transition to the state specified
        /// ---
        /// returns: `true` if it connected atleast one unconnected transition
        pub fn connect(self: *Transition, con: usize) bool {
            var connected = false;
            switch (self.*) {
                .lambda => |*trans| if (trans.* == unconnected) {
                    trans.* = con;
                    connected = true;
                },
                .symbol => |*symbols| for (symbols) |*symbol| {
                    if (symbol.* == unconnected) {
                        connected = true;
                        symbol.* = con;
                    }
                },
            }
            return connected;
        }

        /// Shifts transition refrences by `offs`
        pub fn offset(comptime self: *Transition, offs: usize) void {
            switch (self.*) {
                .lambda => |*trans| if (trans.* != unconnected) {
                    trans.* += offs;
                },
                .symbol => |*symbols| for (symbols) |*symbol| {
                    if (symbol.* != unconnected) {
                        symbol.* += offs;
                    }
                },
            }
        }
    };

    /// This is required to be passed into the regex in order for it to be able to run the analysis
    /// it needs to do
    const ParseState = struct {
        /// The source we are parsing
        src: []const u8,

        /// Where we are in source
        idx: usize = 0,

        /// The backing allocator used to make new nodes
        s: std.BoundedArray(State, 512) = .{ .len = 0 },

        /// Throws a compile error with info of where it happened
        pub inline fn err(comptime self: ParseState, comptime msg: []const u8) noreturn {
            @compileError(std.fmt.comptimePrint(
                "zlex error at \"{s}\": {s}",
                .{ self.src[self.idx..], msg },
            ));
        }

        /// Returns the index of the newly added state
        pub fn add(comptime self: *ParseState, comptime state: State) usize {
            self.*.s.append(state) catch self.err("regex out of NFA s");
            return self.*.s.len - 1;
        }

        /// Returns a pointer to the NState at that index
        pub fn at(comptime self: *ParseState, idx: usize) *State {
            return &self.*.s.buffer[idx];
        }

        /// Connects all unconnected transitions at `from` index to the `to` index
        pub fn con(comptime self: *ParseState, from: usize, to: usize) void {
            const state = self.at(from);
            var new_trans: [state.*.trans.len]Transition = state.*.trans[0..].*;
            @memcpy(&new_trans, state.*.trans);
            for (&new_trans) |*trans| {
                _ = trans.*.connect(to);
            }
            state.*.trans = &new_trans;
        }
    };

    /// Represents an output of a regular expression, with a start and final state
    const Expr = struct {
        /// Index of start state
        start: usize,

        /// Index of final state
        final: usize,

        /// Sets both the start and final index to the index passed in, making the `Expr` a single
        /// state
        pub fn single(expr: usize) Expr {
            return .{ .start = expr, .final = expr };
        }
    };

    /// Represents an NFA node
    const State = struct {
        /// The transitions for this NFA node
        trans: []const Transition,

        /// If specified, this state is a final state
        token_id: ?usize = null,

        /// Adds a transition to the transitions list
        pub fn addTrans(comptime self: *State, trans: Transition) void {
            const final: [self.*.trans.len]Transition = self.*.trans[0..].*;
            const new = final ++ .{trans};
            self.trans = &new;
        }

        /// Shifts every transition to be based off of base_idx, returns new state variable
        pub fn shiftTrans(comptime self: State, base_idx: usize) State {
            var new: [self.*.trans.len]Transition = self.*.trans.*;
            for (&new) |*trans| {
                trans.offset(base_idx);
            }
            const final = new;
            return .{ .trans = final };
        }
    };

    /// Parses a regular expression and turns it into a NFA. If an error occurs, it
    /// throws a `@compileError`
    pub fn parse(comptime src: []const u8, token_id: usize) Nfa {
        // Set branch quota
        @setEvalBranchQuota(src.len * 1000);

        // Parse the regex
        var state = ParseState{ .src = src };
        const expr = parseExpr(&state);
        const states: [state.s.len]State = state.s.buffer[0..state.s.len].*;
        return .{
            .start = expr.start,
            .final = &.{.{
                .idx = expr.final,
                .tok = token_id,
            }},
            .states = &states,
        };
    }

    /// Parses a regular expression and turns it into a NFA. If an error occurs, it
    /// throws a `@compileError`
    /// ---
    /// - `s` internal parser state
    /// ---
    /// returns:
    /// - a `usize` representing the index of the parsed NState in state's backing array
    fn parseExpr(comptime s: *ParseState) Expr {
        const is_grp = s.*.src[s.*.idx] == '(';
        s.*.idx += @intFromBool(is_grp);

        // Every regex starts with a prefix node/instruction
        var prefix = parsePrefix(s);

        // Now we parse nodes that require previous nodes
        var next = parseSuffix(s, prefix);
        while (next) |n| {
            prefix = n;
            next = parseSuffix(s, prefix);
        }

        if (s.*.idx < s.*.src.len and s.src[s.*.idx] != ')') {
            s.err("expected end of expression or group");
        }
        return prefix;
    }

    /// Tries to parse a suffix expression, if it can't it just gives up and returns null
    fn parseSuffix(comptime s: *ParseState, left: Expr) ?Expr {
        if (s.*.idx == s.*.src.len) return null;
        const char = s.*.src[s.*.idx];

        switch (char) {
            '+' => {
                // Add a epsilon transistion back to the start for looping
                s.at(left.final).addTrans(.{ .lambda = left.start });
                s.*.idx += 1;
                return left;
            },
            '*' => {
                // Add a begin and end epsilon 'state'
                const final = s.add(.{ .trans = &.{
                    .{ .lambda = Transition.unconnected },
                } });
                const start = s.add(.{ .trans = &.{
                    .{ .lambda = left.start },
                    .{ .lambda = final },
                } });
                s.con(left.final, final);
                s.at(left.final).*.addTrans(.{ .lambda = left.start });
                s.*.idx += 1;
                return .{ .start = start, .final = final };
            },
            '?', '|' => {
                // Add an epsilon transition around the node and connect the two
                const rejoin = s.add(.{ .trans = &.{
                    .{ .lambda = Transition.unconnected },
                } });
                const split = s.add(.{ .trans = &.{
                    .{ .lambda = left.start },
                    .{ .lambda = rejoin },
                } });
                s.*.idx += 1;

                // If we read the alternation '|' character, then read in the right side instead
                if (char == '|') {
                    const right = parseExpr(s);
                    s.con(right.final, rejoin);
                    s.at(split).*.trans = &.{
                        .{ .lambda = left.start },
                        .{ .lambda = right.start },
                    };
                } else {
                    s.con(left.final, rejoin);
                }

                return .{ .start = split, .final = rejoin };
            },
            else => {
                // Well, we're parsing another terminal/symbol... So lets do concatination
                const symbol = parsePrefix(s);
                s.con(left.final, symbol.start);
                return .{
                    .start = left.start,
                    .final = symbol.final,
                };
            },
        }
    }

    /// Parse a prefix instruction (basically a terminal, or group instruction, etc)
    /// These nodes don't take anything in and only produce output
    fn parsePrefix(comptime s: *ParseState) Expr {
        if (s.*.src[s.*.idx] == '(') {
            // Parse a group expr
            return parse(s);
        } else {
            // Parse a symbol (terminal)
            const range = Range.parse(s.*.src[s.*.idx..]) catch s.err("invalid range format");
            const expr = fromRange(s, range);
            s.*.idx += range.src.len;
            return expr;
        }
    }

    /// Match all within a range
    /// ---
    /// returns:
    /// - a tuple with the input state, and the final state of this terminal
    pub fn fromRange(comptime s: *ParseState, comptime range: Range) Expr {
        if (range.isAscii()) {
            // We only need 1 node for this.
            var t = Transition.nullSymbols();
            for (range.start, range.end) |start_unicode, end_unicode| {
                const start: u8 = @intCast(start_unicode);
                const end = std.math.cast(u8, end_unicode) orelse std.math.maxInt(u8);
                for (start..end + 1) |i| {
                    t.symbol[i] = Transition.unconnected;
                }
            }
            return Expr.single(s.add(.{ .trans = &.{t} }));
        }

        // TODO: Support UTF8
        @compileError("zlex: UTF-8 support is currently not implemented at the moment");
    }

    /// Joins multiple nfa's creating an alternation node at the start (this is to allow multiple
    /// tokens)
    pub fn join(comptime nfas: []const Nfa) Nfa {
        var states = std.BoundedArray(State, blk: {
            var len = 1;
            for (nfas) |nfa| {
                len += nfa.states.len;
            }
            break :blk len;
        }).init(0) catch unreachable;
        var finals = std.BoundedArray(Final, blk: {
            var len = 0;
            for (nfas) |nfa| {
                len += nfa.final.len;
            }
            break :blk len;
        }).init(0) catch unreachable;
        states.append(State{ .trans = &.{} }) catch unreachable;

        var start_state = &states.buffer[0];
        for (nfas) |nfa| {
            const base_state = states.len;
            start_state.addTrans(.{ .lambda = base_state });

            for (nfa.states) |state| {
                try states.append(state.shiftTrans(base_state)) catch unreachable;
            }
            for (nfa.final) |final| {
                try finals.append(.{
                    .idx = final.idx + base_state,
                    .tok = final.tok,
                });
            }
        }

        const final_finals: [finals.buffer.len]Final = finals.buffer;
        const final_states: [states.buffer.len]Final = states.buffer;
        return .{
            .start = 0,
            .final = final_finals,
            .states = final_states,
        };
    }

    /// Finds all NFA nodes that are connected to this one by this transition
    /// ---
    /// - `nfa` the nfa in question
    /// - `state` find states connected to this one
    /// - `on` find all nodes by this transition, if null then epsilon transitions
    /// ---
    /// returns:
    /// - A slice of NFA nodes this connects too
    pub fn getNeighbors(comptime nfa: Nfa, comptime state: usize, comptime on: ?u8) []const usize {
        var seen = std.BoundedArray(usize, 64).init(0) catch unreachable;
        getNeighborsDepth(nfa, state, on, &seen, 0, false);
        const final: [seen.len]usize = seen.slice().*;
        return &final;
    }

    /// Real function to get neighbors.
    /// ---
    /// - `on` find all nodes connected by this transition (or if null, then epsilon transition)
    /// - `seen` already seen nodes.
    /// - `depth` at what depth we are at. If greater than 0, then don't traverse non-epsilon states
    /// - `add` whether or not to add node to `seen`. Used for the first node most of the time.
    /// ---
    /// returns:
    /// - the states present in `seen` and new states found by that transition (all sorted)
    fn getNeighborsDepth(
        comptime nfa: Nfa,
        state: usize,
        on: ?u8,
        seen: *std.BoundedArray(usize, 64),
        depth: usize,
        add: bool,
    ) void {
        // If we're an already seen node, then stop
        const insert_idx = blk: {
            var idx: usize = seen.len / 2;
            var level: usize = seen.len / 4;
            while (level) : (level /= 2) {
                if (state == seen[idx]) {
                    // We already have this so don't search for any more
                    return;
                } else if (state > seen[idx]) {
                    // Move to the left
                    idx -= level;
                } else {
                    // Move to the right
                    idx += level;
                }
            }
            break :blk idx;
        };

        // Using the index of where the node (should) be, we can use that to insert our node
        if (add) seen.insert(insert_idx, state);

        // Recursivly add nodes neighbors
        for (nfa.states[state].trans) |trans| {
            switch (trans) {
                .lambda => |to| getNeighborsDepth(Nfa, to, on, &seen, depth, true),
                .symbol => |bytes| {
                    if (depth < 1) {
                        for (bytes) |to| {
                            getNeighborsDepth(nfa, to, on, &seen, depth + 1, true);
                        }
                    }
                },
            }
        }
    }
};

/// Deterministic finite automata
const Dfa = struct {
    /// What state is the ending state
    final: []const Final,

    /// State transition table
    trans: []const [256]u16,

    /// Error state
    const error_state = 0;

    /// Start state
    const start_state = 1;

    /// Convert a NFA to a DFA
    pub fn fromNfa(nfa: Nfa) Dfa {
        // Set max number of dfa states (this also ensures no state explosion)
        const max_states = nfa.states.len * nfa.states.len * 2;
        const State = struct {
            nfa_states: []const usize,
            token_id: ?usize,
        };
        var states = std.BoundedArray(State, max_states).init(0) catch unreachable;
        var trans = std.BoundedArray([256]u16, max_states).init(0) catch unreachable;

        // Convert the NFA to a DFA

        // Finalize the states and transitions
        const final_trans: [trans.len][256]u16 = trans.slice().*;
        const final_states = get_final_states: {
            var finals = std.BoundedArray(Final, max_states).init(0) catch unreachable;
            for (states.slice(), 0..) |state, idx| {
                if (state.token_id) |tok| {
                    finals.append(.{ .idx = idx, .tok = tok });
                }
            }
            break :get_final_states finals.slice().*;
        };
        return .{
            .final = &final_states,
            .trans = &final_trans,
        };
    }
};

/// Errors that can occur when parsing regex
pub const ParseError = error{
    InvalidRangeFormat,
    InvalidUTF8,
    OutOfMemory,
};



test "Dfa" {
    const nfa = comptime Nfa.parse("(abc)|(def)*", 0);
    @compileLog(comptime nfa.getNeighbors(nfa.start, null));
}
