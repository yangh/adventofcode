local I = require("inspect")
local T = require("tools")

local numbers = T.input_lines_to_number(1)

-- Sort the numbers
table.sort(numbers)

--print(I.inspect(numbers))

-- Build number hash
local num_hash = {}
for _, n in ipairs(numbers) do
        num_hash[n] = 1
end

-- Part 1, find the reminder in the hash
for _, n in ipairs(numbers) do
        local target = 2020 - n
        if num_hash[target] and num_hash[target] == 1 then
                print(n, target, n * target)
                break
        end
end

-- Part 2, find the reminder of the reminder in the hash
local found = false

for _, n in ipairs(numbers) do
        local target = 2020 - n
        for _, n2 in ipairs(numbers) do
                local target2 = target - n2
                if target2 > 0 and num_hash[target2] and num_hash[target2] == 1
                then
                        print(n, n2, target2, n * n2 * target2)
                        found = true
                        break
                end
        end
        if found then break end
end
