build:
	gcc -mwindows main.c -o install.exe
	@#gcc -std=c99 -Wall -Wextra -nostdlib -ffreestanding -mwindows -Os -fno-stack-check -fno-stack-protector -mno-stack-arg-probe main.c -o install.exe -lkernel32 -luser32
	@#gcc -std=c89 -Wall -Wextra -nostdlib -nostartfiles -ffreestanding -mwindows main.c -o install.exe -lkernel32 -luser32

run: build
	./install.exe
