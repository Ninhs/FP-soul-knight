# Vẽ biểu đồ rò rỉ bộ nhớ. Cần: pip install matplotlib
import matplotlib.pyplot as plt

# Số đo "maximum residency" (byte) của từng file
lazy       = 28_072_960   # Lazy.exe
strict     = 15_453_424   # Strict.exe
strict_seq = 42_856       # StrictSeq.exe

def fmt(b):
    return f"{b/1e6:.0f} MB" if b >= 1e6 else f"{b/1e3:.0f} KB"

labels = ["Map lười\n(hiện tại)", "Data.Map.Strict\n(chưa ép mỗi tick)", "Data.Map.Strict\n+ seq mỗi tick"]
values = [lazy, strict, strict_seq]
colors = ["#b22222", "#dd8030", "#2e7d4f"]

fig, ax = plt.subplots(figsize=(8, 4.2), dpi=200)
fig.patch.set_facecolor("#fbf3dc")
ax.set_facecolor("#fbf3dc")
bars = ax.barh(labels, [v / 1e6 for v in values], color=colors, edgecolor="#2b2118", height=0.55)
ax.invert_yaxis()
for bar, v in zip(bars, values):
    ax.text(bar.get_width() + 0.4, bar.get_y() + bar.get_height() / 2, fmt(v),
            va="center", fontsize=12, fontweight="bold", color="#2b2118")
ax.set_xlim(0, max(values) / 1e6 * 1.2)
ax.set_xlabel("Bộ nhớ tối đa (MB)", fontsize=11)
ax.set_title("Rò rỉ bộ nhớ sau 400.000 tick (khoảng 1,9 giờ)", fontsize=14, fontweight="bold", color="#8b1a10")
for s in ("top", "right"):
    ax.spines[s].set_visible(False)
ax.tick_params(labelsize=10)
fig.text(0.5, 0.01, "Mô phỏng vòng lặp phòng: 1 người chơi không tấn công, GHC -O1",
         ha="center", fontsize=8, style="italic", color="#6b5d45")
plt.tight_layout(rect=(0, 0.04, 1, 1))
plt.savefig("bieu_do_ro_ri.png", facecolor=fig.get_facecolor())
print("Đã lưu bieu_do_ro_ri.png")
