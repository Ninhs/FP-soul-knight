# Bot kiểm thử tải cho server FP Soul Knight.
# Mỗi bot là một client giả: vào phòng, chọn hero, sẵn sàng, bắt đầu ván rồi gửi thao tác liên tục.
# Cứ 4 bot một phòng (tối đa của game). Cần: pip install websockets
import argparse
import asyncio
import json
import math
import os
import random
import statistics
import time
import urllib.request

import websockets

ROOM_SIZE = 4


class Stats:
    def __init__(self):
        self.connected = 0
        self.failed = []          # (tên bot, lý do)
        self.join_ms = []         # từ lúc gửi JoinRoom đến lúc nhận RoomJoined
        self.rooms_started = 0
        self.start_errors = []
        self.updates = {}         # tên bot -> số StateUpdate nhận được
        self.window = {}          # tên bot -> [lúc bắt đầu nhận, lúc kết thúc]
        self.bytes = 0
        self.dropped = []         # bot bị đứt kết nối giữa chừng


async def bot(idx, args, stats, room_codes, joined_counts):
    name = f"{args.prefix}_{idx:03d}"
    group = idx // ROOM_SIZE
    leader = idx % ROOM_SIZE == 0
    stats.updates[name] = 0
    await asyncio.sleep((idx / args.ramp) if args.ramp > 0 else 0)  # tăng dần số bot mỗi giây
    try:
        ws = await websockets.connect(args.url, open_timeout=30, max_size=None)
    except Exception as e:
        stats.failed.append((name, f"không kết nối được: {e!r}"))
        if leader:
            room_codes[group].set_exception(RuntimeError("leader lỗi"))
        return
    try:
        # 1. Vào phòng (người đầu nhóm tạo phòng, mã rỗng; ba người sau dùng mã của phòng đó)
        if leader:
            code = ""
        else:
            code = await asyncio.wait_for(room_codes[group], timeout=60)
        t0 = time.monotonic()
        await ws.send(json.dumps({"tag": "JoinRoom", "roomCode": code, "playerName": name}))
        raw = await asyncio.wait_for(ws.recv(), timeout=30)
        msg = json.loads(raw)
        if msg.get("tag") != "RoomJoined":
            stats.failed.append((name, f"không vào được phòng: {raw[:120]}"))
            if leader:
                room_codes[group].set_exception(RuntimeError("leader lỗi"))
            return
        stats.join_ms.append((time.monotonic() - t0) * 1000)
        stats.connected += 1
        if leader:
            room_codes[group].set_result(msg["joinedCode"])
        joined_counts[group] += 1

        # 2. Chọn hero, sẵn sàng
        await ws.send(json.dumps({"tag": "ChooseRole", "contents": random.choice(["Warrior", "Archer"])}))
        await ws.send(json.dumps({"tag": "SetReady", "contents": True}))

        # Đọc tin từ server ở một tác vụ riêng
        async def reader():
            try:
                async for m in ws:
                    stats.bytes += len(m)
                    if '"StateUpdate"' in m[:40]:
                        stats.updates[name] += 1
            except Exception:
                stats.dropped.append(name)

        stats.window[name] = [time.monotonic(), None]
        reader_task = asyncio.create_task(reader())

        # 3. Chủ phòng chờ cả nhóm vào đủ rồi bắt đầu
        group_size = min(ROOM_SIZE, args.bots - group * ROOM_SIZE)
        if leader:
            while joined_counts[group] < group_size:
                await asyncio.sleep(0.2)
            await asyncio.sleep(2)  # để mọi người kịp bấm sẵn sàng
            await ws.send(json.dumps({"tag": "StartGame"}))
            stats.rooms_started += 1

        # 4. Gửi thao tác ~20 lần/giây: đi theo hướng ngẫu nhiên và liên tục đánh
        end = time.monotonic() + args.duration
        angle = random.uniform(0, 2 * math.pi)
        next_turn = 0.0
        while time.monotonic() < end and not reader_task.done():
            now = time.monotonic()
            if now >= next_turn:
                angle = random.uniform(0, 2 * math.pi)
                next_turn = now + random.uniform(0.5, 1.5)
            await ws.send(json.dumps({
                "tag": "InputMsg",
                "moveDir": [math.cos(angle), math.sin(angle)],
                "aimDir": [1.0, 0.0],
                "attack": True,
            }))
            await asyncio.sleep(0.05)

        stats.window[name][1] = time.monotonic()
        await ws.send(json.dumps({"tag": "LeaveRoom"}))
        reader_task.cancel()
    except Exception as e:
        stats.failed.append((name, f"lỗi trong lúc chạy: {e!r}"))
    finally:
        await ws.close()


async def main():
    p = argparse.ArgumentParser(description="Bot kiểm thử tải cho FP Soul Knight")
    p.add_argument("--url", default="ws://127.0.0.1:8080/", help="địa chỉ server, ví dụ wss://fp-soul-knight.onrender.com/")
    p.add_argument("--bots", type=int, default=100, help="số bot (4 bot một phòng)")
    p.add_argument("--duration", type=float, default=60, help="số giây mỗi bot chơi")
    p.add_argument("--ramp", type=float, default=5, help="số bot kết nối thêm mỗi giây (0 = tất cả cùng lúc)")
    p.add_argument("--prefix", default="bot", help="tiền tố tên bot (dùng để xóa dữ liệu thử sau này)")
    p.add_argument("--slack-webhook", default=os.environ.get("SLACK_WEBHOOK_URL"), help="địa chỉ Incoming Webhook của Slack để gửi kết quả (hoặc đặt biến môi trường SLACK_WEBHOOK_URL)")
    args = p.parse_args()

    stats = Stats()
    groups = (args.bots + ROOM_SIZE - 1) // ROOM_SIZE
    loop = asyncio.get_running_loop()
    room_codes = [loop.create_future() for _ in range(groups)]
    joined_counts = [0] * groups

    print(f"Chạy {args.bots} bot ({groups} phòng) tới {args.url}, mỗi bot chơi {args.duration:.0f} giây")
    t0 = time.monotonic()
    await asyncio.gather(*(bot(i, args, stats, room_codes, joined_counts) for i in range(args.bots)))
    total = time.monotonic() - t0

    rates = [stats.updates[n] / (w[1] - w[0]) for n, w in stats.window.items() if w[1] and w[1] > w[0] and stats.updates[n] > 0]
    lines = [
        f"Kiểm thử tải FP Soul Knight: {args.bots} bot ({groups} phòng), mỗi bot chơi {args.duration:.0f} giây, server {args.url}",
        f"Tổng thời gian chạy       : {total:.1f} giây",
        f"Bot vào phòng thành công  : {stats.connected}/{args.bots}",
        f"Bot lỗi                   : {len(stats.failed)}",
        f"Bot bị đứt kết nối        : {len(stats.dropped)}",
        f"Phòng đã bấm bắt đầu      : {stats.rooms_started}/{groups}",
    ]
    if stats.join_ms:
        sj = sorted(stats.join_ms)
        lines.append(f"Thời gian vào phòng (ms)  : trung bình {statistics.mean(sj):.0f}, lớn nhất {sj[-1]:.0f}, p95 {sj[max(0, int(len(sj) * 0.95) - 1)]:.0f}")
    if rates:
        lines.append(f"StateUpdate mỗi bot (/giây): trung bình {statistics.mean(rates):.1f}, thấp nhất {min(rates):.1f}  (server gửi 30/giây)")
    lines.append(f"Dữ liệu nhận về           : {stats.bytes / 1e6:.1f} MB")
    for name, why in stats.failed[:10]:
        lines.append(f"  - {name}: {why}")
    if len(stats.failed) > 10:
        lines.append(f"  ... và {len(stats.failed) - 10} bot lỗi khác")

    report = "\n".join(lines)
    print("\n===== KẾT QUẢ =====")
    print(report)
    if args.slack_webhook:
        send_slack(args.slack_webhook, report)


def send_slack(webhook, text):
    body = json.dumps({"text": "```\n" + text + "\n```"}).encode("utf-8")
    req = urllib.request.Request(webhook, data=body, headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=15) as r:
            print(f"\nĐã gửi kết quả vào Slack (HTTP {r.status})")
    except Exception as e:
        print(f"\nKhông gửi được vào Slack: {e!r}")


if __name__ == "__main__":
    asyncio.run(main())
