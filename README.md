# FP Soul Knight

Game hành động nhiều người chơi lấy ý tưởng từ **Soul Knight**, viết hoàn toàn bằng **Haskell** cho môn Lập trình hàm (Functional Programming) tại UEH.

Người chơi đi qua các phòng, diệt quái, hạ boss và nâng cấp nhân vật. Có thể chơi **offline** một mình hoặc **online** tối đa **4 người** trong cùng một phòng.

## Tính năng

- Hai hero: **Swordsman** (cận chiến) và **Archer** (tầm xa). Mỗi hero có cấp nâng cấp riêng (máu, sát thương, tốc độ).
- Ba phòng mỗi ván: phòng đầu, phòng quái nhỏ và phòng boss. Dọn hết quái, mở rương rồi bước vào cổng để sang phòng sau.
- Cửa hàng (Shop) mua đồ trong ván, và màn Upgrade Stats nâng cấp lâu dài bằng gem.
- Chơi online qua phòng có mã: để trống mã khi bấm JOIN sẽ tạo phòng mới, nhập mã để vào phòng có sẵn. Chủ phòng bấm bắt đầu khi mọi người đã sẵn sàng.
- Vàng, EXP, gem và cấp nâng cấp của người chơi online được lưu vào cơ sở dữ liệu PostgreSQL.

## Điều khiển

| Phím / chuột | Tác dụng |
|---|---|
| `W` `A` `S` `D` (hoặc phím mũi tên) | Di chuyển |
| Giữ chuột trái | Đánh kẻ địch gần nhất trong tầm |
| `E` | Mở rương, tương tác với vật thể gần đó |

## Kiến trúc

Dự án gồm ba package cabal trong cùng một workspace (`cabal.project`):

- `shared/`: kiểu dữ liệu và giao thức dùng chung cho client và server. Mã nguồn trong `shared/src` gồm `Types/` (Player, Monster, Item, GameState), `Protocol`, `MapLayout`, `StageLayout`.
- `server/`: server game (WebSocket và PostgreSQL). Mã nguồn trong `server/src` gồm `Main`, `Network`, `Room`, `RoomLoop`, `DB`, `Config` và thư mục `Logic/` (AI, Combat, Control, GameLoop, MapGen, Movement, Portal).
- `client/`: client đồ họa dùng gloss. Mã nguồn trong `client/src` gồm `Main`, `Network`, `Render`, `Input`, `Assets`, `Sprites`, thư mục `Views/` và `Components/`.
- `experiments/`: kiểm thử kèm báo cáo, gồm đo rò rỉ bộ nhớ do lazy evaluation (`leak_check`) và kiểm thử tải bằng bot (`load_test`).
- `Dockerfile`: đóng gói server để triển khai.
- `cabal.project`: khai báo ba package `shared`, `server`, `client`.

### Cách hoạt động

- **Client** vẽ màn hình ở 60 khung hình/giây, đọc phím và chuột rồi gửi thao tác lên server. Ở chế độ offline, client tự chạy luật game bằng các module `Logic` của server.
- **Server** giữ toàn bộ luật chơi ở chế độ online: mỗi phòng có một luồng game chạy **60 tick/giây** (di chuyển, chiến đấu, quái, đạn), gửi trạng thái cho client **30 lần/giây** và tự lưu vàng, EXP khoảng mỗi 5 giây.
- **Giao thức**: client và server nói chuyện qua **WebSocket**, thông điệp là JSON (thư viện `aeson`) với các kiểu định nghĩa trong `shared/src/Protocol.hs`. Port 443 dùng `wss://` (TLS), các port khác dùng `ws://`.
- **Cơ sở dữ liệu**: PostgreSQL (triển khai trên Neon). Server tự tạo bảng khi khởi động. Mỗi thao tác đọc hoặc ghi dùng một kết nối riêng.

### Thiết kế song song và đồng bộ hóa

- Server chạy bốn loại luồng: luồng nhận kết nối, một luồng cho mỗi người chơi, một luồng game cho mỗi phòng, và luồng ghi database.
- Client chạy hai luồng: luồng giao diện (gloss) và luồng mạng. Luồng mạng ghi trạng thái mới nhất vào một `MVar`, luồng giao diện đọc ra để vẽ.
- Trạng thái mỗi phòng nằm trong các `TVar`, mọi thay đổi chạy trong giao dịch **STM** nên không có tình trạng hai luồng ghi đè lên nhau và không bị deadlock.

## Yêu cầu

- GHC **9.10.3** và `cabal` (cài bằng [GHCup](https://www.haskell.org/ghcup/)).
- Để build/chạy **server** cục bộ: PostgreSQL (hoặc dùng Docker, xem bên dưới) và thư viện `libpq` cùng `pkg-config`.
- Để build **client**: không cần gì thêm trên Windows. Trên Linux cần `freeglut` và các thư viện OpenGL.

## Chạy game

### Cách 1: chơi trên server có sẵn

1. Tải bản phát hành trong mục **Releases** của repo, giải nén.
2. Kiểm tra file `server.txt` cạnh file chạy có dòng `fp-soul-knight.onrender.com:443`.
3. Chạy `client.exe`.

Server chạy trên gói miễn phí nên có thể "ngủ" khi lâu không có người dùng. Lần đầu kết nối đợi vài chục giây để nó thức dậy.

### Cách 2: build từ mã nguồn

```bash
cabal build client
cabal run client
```

Client đọc địa chỉ server theo thứ tự: dòng đầu của file `server.txt` (dạng `host:port`), rồi biến môi trường `SK_SERVER`, cuối cùng là mặc định `127.0.0.1:8080`. Chạy lệnh từ thư mục gốc của repo để client tìm thấy thư mục `client/assets`.

### Chạy server cục bộ

Server đọc hai biến môi trường:

| Biến | Ý nghĩa | Mặc định |
|---|---|---|
| `DATABASE_URL` | Chuỗi kết nối PostgreSQL | `postgresql://postgres:postgres@localhost:5432/soulknight` |
| `PORT` | Cổng lắng nghe | `8080` |

```bash
cabal run server
```

Hoặc dùng Docker (không cần cài `libpq`):

```bash
docker build -t fp-soul-knight-server .
docker run -p 8080:8080 -e DATABASE_URL="postgresql://USER:PASSWORD@HOST:5432/DBNAME" fp-soul-knight-server
```

Sau đó đặt `127.0.0.1:8080` vào `server.txt` rồi chạy client. Server cũng trả lời HTTP 200 với yêu cầu thường nên có thể mở `http://localhost:8080` bằng trình duyệt để kiểm tra server còn sống.

> Không đưa mật khẩu hoặc chuỗi kết nối thật vào repo. Triển khai thật nên đặt `DATABASE_URL` trong phần biến môi trường của nơi lưu trữ server của bạn (ví dụ Render).

## Kiểm thử

Thư mục `experiments/` chứa hai phần kiểm thử đi kèm báo cáo, mỗi phần có README riêng:

- [`experiments/leak_check`](experiments/leak_check): mô phỏng đo **rò rỉ bộ nhớ do lazy evaluation** với bảng thời gian hồi đòn của người chơi. So sánh `Data.Map` lười, `Data.Map.Strict` và `Data.Map.Strict` kèm `seq` sau 400.000 tick (khoảng 1,9 giờ chơi).
- [`experiments/load_test`](experiments/load_test): script `bot_test.py` tạo nhiều bot giả làm người chơi (cứ 4 bot một phòng) để kiểm tra server chịu tải, ví dụ 100 bot cùng lúc. In ra số bot vào được phòng, thời gian vào phòng và số bản cập nhật mỗi bot nhận được mỗi giây.

Ngoài ra, các chức năng online (tạo phòng, vào phòng, chọn hero, bắt đầu ván) được kiểm thử thủ công với vài client thật.

## Hạn chế hiện tại

- Gem và cấp nâng cấp do client gửi lên và server chưa kiểm tra lại, nên chưa chống được gian lận.
- Đồ mua trong Shop (bùa sát thương, giày) chỉ có tác dụng trong một ván, chưa lưu lâu dài.
- Hero đang chọn chưa được lưu vào database.
- Tiến trình chơi offline không được lưu.
- Chưa có bộ kiểm thử đơn vị tự động. Phần online được kiểm thử thủ công và bằng bot tạo tải trong `experiments/load_test`.

## Nhóm thực hiện

Dự án cuối kỳ môn Lập trình hàm, Trường Đại học Kinh tế TP. Hồ Chí Minh (UEH).

| Thành viên | MSSV | Phụ trách |
|---|---|---|
| Đỗ Đăng Ninh (Leader) | 31241024923 | Server, database, game state |
| Lâm Việt Tân | 31241024488 | Input game, game loop |
| Nguyễn Anh Tú | 31241022873 | Đồ họa, asset, giao diện |
| Nguyễn Quốc Huy | 31241026529 | AI quái |
