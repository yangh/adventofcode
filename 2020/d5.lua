local I = require("inspect")
local T = require("tools")

local lines, count = T.input(5)

local LOWER = 0
local UPPER = 1

local function convert_line(line)
        local rows = {}
        local cols = {}

        for i = 1, 7 do
                local ch = string.sub(line, i, i)
                if ch == "F" then
                        table.insert(rows, LOWER)
                elseif ch == "B" then
                        table.insert(rows, UPPER)
                end
        end

        for i = 8, 10 do
                local ch = string.sub(line, i, i)
                if ch == "L" then
                        table.insert(cols, LOWER)
                elseif ch == "R" then
                        table.insert(cols, UPPER)
                end
        end

        return rows, cols
end

local function binary_search(items, startn, endn)
        local startx, endx = startn, endn

        for _, binary in ipairs(items) do
                --print("Search", startx, endx)
                if (endx - startx) == 1 then
                        if binary == LOWER then
                                return startx
                        elseif binary == UPPER then
                                return endx
                        end
                end

                local half = (endx - startx + 1) / 2
                if binary == LOWER then
                        endx = startx + half - 1
                elseif binary == UPPER then
                        startx = startx + half
                end
        end

        return nil
end

local function exe_line(line)
        local rows, cols = convert_line(line)
        local row = binary_search(rows, 0, 127)
        local col = binary_search(cols, 0, 7)

        --print(line, row, col, I.inspect(rows), I.inspect(cols))

        return (8 * row) + col
end

--print("UT", exe_line("BFFFBBFRRR"))

-- Part 1
local seat_indexes = T.map_lines(lines, exe_line)
table.sort(seat_indexes, function (a, b) return a > b end)
print("Max seat index", seat_indexes[1])
