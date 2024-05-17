using ECharts
x1 = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
x2 = [11, 11, 15, 13, 12, 13, 10]
b = bar(x1, x2)

y1 = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
y2 = [11, 11, 15, 13, 12, 13, 10]
bch = bar(y1, y2, color = "lightgray", horizontal = true)