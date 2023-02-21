-- See also Page 22 in Programming in Lua, 4th Editor
local binary_operators = {
    { "or" },
    { "and" },
    { "<", ">", "<=", ">=", "~=", "==" },
    { "|" },
    { "~" },
    { "&" },
    { "<<", ">>" },
    -- { ".." },
    { "+", "-" },
    { "*", "/", "//", "%" },
}

local unary_operators = {
    { "not", "-", "#", "~"},
}

local Lexer = require("lexer")
local take = Lexer.next

local Parser = {}

function Parser.new(input, handler)
    local self = Lexer.new(input) -- Inherit from lexer
    self.handler = handler
    return self
end

local function syntax_error(parser, message)
    error("syntax error: " .. message .. " at " .. parser.filename .. ":" .. parser.line)
end

local function expect(parser, token)
    if parser.current ~= token then
        syntax_error(parser, "expecting " .. token)
    end
end

local function expect_and_take(parser, token)
    expect(parser, token)
    take(token)
end

local function table_find(t, ele)
    for i = 1,#t do
        if ele == t[i] then return i end
    end

    return nil
end

local parse_atom
local parse_postfix
local parse_expr
local parse_parameters
local parse_unary
local parse_binary
local parse_binary_

-- Left recursion elimination
-- E := E + T
-- ==>
--    E  := T E'
--    E' := nil | + E

parse_binary = function(self, level)
    if level == nil then level = 1 end
    local ops = binary_operators[level]

    if ops == nil then
        return parse_unary(self)
    end

    local a = parse_binary(self, level+1)
    return parse_binary_(self, level, ops, a)
end

parse_binary_ = function(self, level, ops, a)
    local token = self.current
    local handler = self.handler
    if table_find(ops, token) ~= nil then
        take(self)
        local b = parse_binary(self, level+1)
        return parse_binary_(self, level, ops, handler.binary(token, a, b))
    else
        return a
    end
end

parse_unary = function(self, level)
    if level == nil then level = 1 end
    local ops = unary_operators[level]

    if ops == nil then
        return parse_atom(self)
    end

    local handler = self.handler
    local token = self.current

    if table_find(ops, token) ~= nil then
        take(self)
        return handler.unary(token, parse_unary(self, level+1))
    else
        return parse_unary(self, level+1)
    end
end

parse_atom = function(self)
    local token = self.current
    local handler = self.handler
    if token == "(" then
        local expr = parse_expr(self)
        expect_and_take(")")
        return expr
    elseif type(token) == "string" then
        local id = handler.identify(self, token)
        take(self)
        return parse_postfix(self, id)
    elseif type(token) == "number" or type(token) == "boolean" then
        take(self)
        return token
    end

    syntax_error(self, "expecting atom, get " .. tostring(token))
end

parse_postfix = function(self, id)
    local token = self.current
    local handler = self.handler

    if token == "." then
        take(self)
        token = self.current
        take(self)
        return handler.field_access(self, id, token)
    elseif token == "[" then
        take(self)
        local index = parse_expr(self)
        expect_and_take("]")
        return handler.index_access(self, id, index)
    elseif token == "(" then
        take(self)
        local parameters = parse_parameters(self)
        expect_and_take(")")
        return handler.call(id, parameters)
    end
end

-- Tests

local function test_expr()
    local code = "1 and 2 * 2 - 3 + -2e-3 * not 0xFF"
    local handler = {}
    handler.binary = function (op, a, b)
        local r = table.concat{"(", op, " ", a, " ", b, ")"}
        print(r)
        return r
    end

    handler.unary = function (op, a)
        local r = table.concat{"(", op, " ", a, ")"}
        print(r)
        return r
    end

    local parser = Parser.new(code, handler)
    assert(parse_binary(parser) == "(and 1 (+ (- (* 2 2) 3) (* (- 0.002) (not 255))))")
end

test_expr()
