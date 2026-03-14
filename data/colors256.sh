#!/bin/sh
# 256色 / TrueColor 表示テスト

echo "=== 基本8色 (SGR 30-37) ==="
for i in $(seq 30 37); do
  printf "\033[${i}m %-6s \033[0m" "${i}"
done
echo ""

echo "=== 明るい色 (SGR 90-97) ==="
for i in $(seq 90 97); do
  printf "\033[${i}m %-6s \033[0m" "${i}"
done
echo ""

echo "=== 背景: 基本8色 (SGR 40-47) ==="
for i in $(seq 40 47); do
  printf "\033[${i}m %-6s \033[0m" "${i}"
done
echo ""

echo "=== 背景: 明るい色 (SGR 100-107) ==="
for i in $(seq 100 107); do
  printf "\033[${i}m %-6s \033[0m" "${i}"
done
echo ""

echo ""
echo "=== 256色パレット (SGR 38;5;N) ==="
echo "--- 標準16色 (0-15) ---"
for i in $(seq 0 15); do
  printf "\033[48;5;${i}m\033[38;5;$((i > 6 && i < 9 ? 0 : 15))m %3d \033[0m" "$i"
  [ $(( (i + 1) % 8 )) -eq 0 ] && echo ""
done

echo "--- 6x6x6 カラーキューブ (16-231) ---"
for i in $(seq 16 231); do
  printf "\033[48;5;${i}m  \033[0m"
  [ $(( (i - 15) % 36 )) -eq 0 ] && echo ""
  [ $(( (i - 15) % 6 )) -eq 0 ] && [ $(( (i - 15) % 36 )) -ne 0 ] && printf " "
done
echo ""

echo "--- グレースケール (232-255) ---"
for i in $(seq 232 255); do
  printf "\033[48;5;${i}m  \033[0m"
done
echo ""

echo ""
echo "=== TrueColor (SGR 38;2;R;G;B) ==="
echo "--- 赤グラデーション ---"
for i in $(seq 0 8 255); do
  printf "\033[48;2;${i};0;0m \033[0m"
done
echo ""

echo "--- 緑グラデーション ---"
for i in $(seq 0 8 255); do
  printf "\033[48;2;0;${i};0m \033[0m"
done
echo ""

echo "--- 青グラデーション ---"
for i in $(seq 0 8 255); do
  printf "\033[48;2;0;0;${i}m \033[0m"
done
echo ""

echo "--- 虹 ---"
for i in $(seq 0 3 255); do
  if [ $i -lt 85 ]; then
    r=$((i * 3)); g=$((255 - i * 3)); b=0
  elif [ $i -lt 170 ]; then
    r=$((255 - (i - 85) * 3)); g=0; b=$(((i - 85) * 3))
  else
    r=0; g=$(((i - 170) * 3)); b=$((255 - (i - 170) * 3))
  fi
  printf "\033[48;2;${r};${g};${b}m \033[0m"
done
echo ""

echo ""
echo "=== 組み合わせテスト ==="
printf "\033[1;38;2;255;100;0mBold + TrueColor(255,100,0)\033[0m\n"
printf "\033[4;38;5;196mUnderline + 256color(196)\033[0m\n"
printf "\033[38;5;82;48;5;17m 256fg(82) on 256bg(17) \033[0m\n"
printf "\033[38;2;255;255;0;48;2;0;0;128m TrueColor fg(255,255,0) on bg(0,0,128) \033[0m\n"
echo ""
echo "Done!"
