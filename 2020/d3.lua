local I = require("inspect")
local T = require("tools")

local lines, count = T.input(3)

local width = # lines[1]
local height = count

local xstep = 3
local ystep = 1

local x, y = 1, 1
local TREE = string.byte("#", 1)

--print("Input size (w/h)", width, height)

local function step_is_tree(line)
        local current_x, current_y = x, y

        x = x + xstep
        y = y + ystep

        if x > width then x = x - width end

        if current_y == 1  or current_y > height then
                return false
        end

        return (string.byte(lines[current_y], current_x) == TREE)
end

-- Part 1
print("Trees on the way", T.fold_lines(lines, step_is_tree))

-- Part 2
local slopes = {
        { 1, 1 },
        { 3, 1 },
        { 5, 1 },
        { 7, 1 },
        { 1, 2 },
}

local multied = 1
for _, slope in ipairs(slopes) do
        x, y = 1, 1
        xstep, ystep = slope[1], slope[2]
        --print("Check slope", xstep, ystep)
        multied = multied * T.fold_lines(lines, step_is_tree)
end

print("Multied slopes", multied)