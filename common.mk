warning_flags = -Wall -Wextra -Wformat=2 -Winit-self -Wshadow\
		-Wpointer-arith -Wcast-qual -Wcast-align\
		-Wlogical-op -Wredundant-decls\
		-Wzero-as-null-pointer-constant\
		-pedantic

test_flags = -Wconversion -D_FORTIFY_SOURCE=2
test_envs = MALLOC_CHECK_=2 
