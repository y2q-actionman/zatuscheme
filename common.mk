warning_flags = -Wall -Wextra -Wformat=2 -Winit-self -Wshadow\
		-Wpointer-arith -Wcast-qual -Wcast-align\
		-Wlogical-op -Wredundant-decls\
		-pedantic\
		-fno-nonansi-builtins
#		-Wzero-as-null-pointer-constant

test_flags = -D_FORTIFY_SOURCE=2 -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC
#	-Wconversion 

test_envs = MALLOC_CHECK_=2 
