import os
from PIL import Image

# Lấy chính xác thư mục chứa file convert.py hiện tại
script_dir = os.path.dirname(os.path.abspath(__file__))

# Đường dẫn tới thư mục client/assets
target_dir = os.path.join(script_dir, "client", "assets")

print(f"Đang quét thư mục: {target_dir}")

if not os.path.exists(target_dir):
    print("❌ Không tìm thấy thư mục client/assets! Kiểm tra lại vị trí đặt file script.")
else:
    count = 0
    # os.walk quét sâu vào tất cả các thư mục con (archer, demon, monster, orc, soldier...)
    for root, dirs, files in os.walk(target_dir):
        for file in files:
            # Kiểm tra định dạng .png (bao gồm cả .PNG viết hoa)
            if file.lower().endswith(".png"):
                png_path = os.path.join(root, file)
                bmp_path = os.path.splitext(png_path)[0] + ".bmp"
                
                try:
                    # Mở file PNG và lưu thành BMP
                    img = Image.open(png_path)
                    img.save(bmp_path, "BMP")
                    
                    # Xóa file PNG gốc sau khi chuyển đổi thành công
                    os.remove(png_path)
                    
                    print(f"Đã chuyển và xóa: {bmp_path}")
                    count += 1
                except Exception as e:
                    print(f"Lỗi khi xử lý {png_path}: {e}")

    print(f"Hoàn tất! Đã chuyển đổi và dọn dẹp tổng cộng {count} file.")