local I = require("inspect")
local T = require("tools")

local lines = T.input(2)

local function password_valid(line)
        local smin, smax, schar, pwd = string.match(line, "(%d+)-(%d+) (%w): (%w+)")
        local min = tonumber(smin)
        local max = tonumber(smax)
        local char = string.byte(schar, 1)
        local char_count = 0

        for i = 1, #pwd do
                if char == string.byte(pwd, i) then
                        char_count = char_count + 1
                end
        end
        --print(min, max, schar, char_count, pwd)

        return (char_count >= min and char_count <= max)
end

-- Part 1, count valid password
print("Valid password", T.fold_lines(lines, password_valid))

local function password_valid2(line)
        local smin, smax, schar, pwd = string.match(line, "(%d+)-(%d+) (%w): (%w+)")
        local min = tonumber(smin)
        local max = tonumber(smax)
        local char = string.byte(schar, 1)
        local char_count = 0
        local pwd_len = # pwd

        if char == string.byte(pwd, min) then
                char_count = char_count + 1
        end
        if max <= pwd_len and char == string.byte(pwd, max) then
                char_count = char_count + 1
        end
        --print(min, max, schar, char_count, pwd)

        return (char_count == 1)
end

-- Part 2, count valid password
print("Valid password", T.fold_lines(lines, password_valid2))