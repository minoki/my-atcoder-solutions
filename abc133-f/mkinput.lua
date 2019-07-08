io.output("input1.txt")
do
  local n = 10
  local q = 5
  io.write(string.format("%d %d\n", n, q))
  for i = 1, n-1 do
    io.write(string.format("%d %d %d %d\n", i, i+1, math.random(1, 5), math.random(10, 100)))
  end
  for i = 1, q do
    local u = math.random(1, n-1)
    local v = math.random(u+1, n)
    io.write(string.format("%d %d %d %d\n", math.random(1, 5), math.random(10, 100), u, v))
  end
end

io.output("input2.txt")
do
  local left = 5
  local right = 5
  local n = left + right - 1
  local q = 5
  io.write(string.format("%d %d\n", n, q))
  for i = 1, left-1 do
    io.write(string.format("%d %d %d %d\n", i, i+1, math.random(1, 5), math.random(10, 100)))
  end
  for i = 1, right-1 do
    local a = i == 1 and i or i + left - 1
    io.write(string.format("%d %d %d %d\n", a, i + left, math.random(1, 5), math.random(10, 100)))
  end
  for i = 1, q do
    local u = math.random(1, n-1)
    local v = math.random(u+1, n)
    io.write(string.format("%d %d %d %d\n", math.random(1, 5), math.random(10, 100), u, v))
  end
end
