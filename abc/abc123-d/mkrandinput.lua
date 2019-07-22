math.randomseed(os.time())

local x = 1000
local y = 1000
local z = 1000

local k = math.min(3000, x * y * z)

io.output("randinput.txt")

io.write(string.format("%d %d %d %d\n", x, y, z, k))
do
  local a = {}
  for i = 1, x do
    table.insert(a, math.random(10^10-1, 10^10))
  end
  io.write(table.concat(a, " "), "\n")
end

do
  local a = {}
  for i = 1, y do
    table.insert(a, math.random(10^10-1, 10^10))
  end
  io.write(table.concat(a, " "), "\n")
end

do
  local a = {}
  for i = 1, z do
    table.insert(a, math.random(10^10-1, 10^10))
  end
  io.write(table.concat(a, " "), "\n")
end

