# Kiểm thử tải bằng bot

Script `bot_test.py` tạo nhiều **bot** (client giả, hay còn gọi là virtual user) kết nối vào server FP Soul Knight cùng lúc để xem server chịu được bao nhiêu người chơi. Không cần 100 người thật, một máy có thể chạy hàng trăm bot.

## Mỗi bot làm gì

1. Kết nối WebSocket tới server, gửi `JoinRoom` với tên `bot_000`, `bot_001`, ... Cứ 4 bot một phòng (tối đa của game): bot đầu nhóm tạo phòng, ba bot còn lại vào bằng mã phòng đó.
2. Chọn hero ngẫu nhiên (Swordsman hoặc Archer) và bấm sẵn sàng.
3. Bot đầu nhóm bấm bắt đầu khi cả nhóm đã vào phòng.
4. Trong suốt thời gian test, mỗi bot gửi thao tác di chuyển và đánh 20 lần/giây, giống người chơi thật.
5. Hết giờ thì rời phòng và ngắt kết nối.

Bot được thêm dần (mặc định 5 bot/giây) để không dồn hết yêu cầu vào cùng một lúc.

## Cách chạy

Cần Python 3.9 trở lên.

```bash
pip install websockets
python bot_test.py --url wss://fp-soul-knight.onrender.com/ --bots 20 --duration 30
```

Server chạy trên gói miễn phí nên có thể đang "ngủ". Mở `https://fp-soul-knight.onrender.com` bằng trình duyệt và đợi hiện chữ `OK` trước khi chạy.

| Tham số | Ý nghĩa | Mặc định |
|---|---|---|
| `--url` | Địa chỉ server (`wss://` cho Render, `ws://127.0.0.1:8080/` cho máy cục bộ) | bắt buộc |
| `--bots` | Số bot | 100 |
| `--duration` | Số giây mỗi bot chơi | 60 |
| `--ramp` | Số bot thêm mỗi giây | 5 |
| `--prefix` | Tiền tố tên bot | `bot` |
| `--slack-webhook` | Gửi kết quả vào kênh Slack (không bắt buộc, có thể đặt bằng biến môi trường `SLACK_WEBHOOK_URL`) | không có |

Nên tăng dần 20, 50 rồi 100 bot để biết mức nào bắt đầu có vấn đề.

## Đọc kết quả

Cuối lần chạy, script in khối **KẾT QUẢ**:

| Chỉ số | Ý nghĩa |
|---|---|
| Bot vào phòng thành công | Số bot vào được phòng, càng gần tổng số bot càng tốt |
| Bot lỗi | Bot không kết nối hoặc không vào được phòng, kèm lý do |
| Bot bị đứt kết nối | Bot đã vào nhưng bị server ngắt giữa chừng |
| Phòng đã bấm bắt đầu | Số phòng bắt đầu ván thành công |
| Thời gian vào phòng | Từ lúc gửi `JoinRoom` đến lúc nhận trả lời (trung bình, lớn nhất, p95) |
| StateUpdate mỗi bot | Số bản cập nhật trạng thái mỗi bot nhận mỗi giây. Server gửi 30/giây, số này tụt xuống nghĩa là server không theo kịp |
| Dữ liệu nhận về | Tổng lượng dữ liệu các bot nhận |

## Kết quả đo

Điền sau khi chạy trên server thật.

| Số bot | Số phòng | Vào phòng thành công | Bot lỗi | Bị đứt | StateUpdate/giây (trung bình) | Thời gian vào phòng p95 (ms) |
|---|---|---|---|---|---|---|
| 20 | 5 | | | | | |
| 50 | 13 | | | | | |
| 100 | 25 | | | | | |

## Lưu ý

- Bot chạy trên máy của bạn nên kết quả còn phụ thuộc đường truyền mạng và gói miễn phí của Render (CPU thấp, có thể bị giới hạn). Đây là phép đo thực tế chứ không phải giới hạn tối đa của thiết kế.
- Mỗi bot tạo một dòng trong bảng `players` của database. Sau khi test, xóa các dòng này (bảng `session_players` tự xóa theo vì có `ON DELETE CASCADE`):

```sql
DELETE FROM players WHERE name LIKE 'bot\_%';
DELETE FROM game_sessions WHERE id NOT IN (SELECT session_id FROM session_players);
```

- Không đưa địa chỉ Slack webhook vào repo. Ai có địa chỉ đó đều đăng được vào kênh của bạn.
- Chỉ test trên server của nhóm. Không chạy script này nhắm vào server của người khác.
