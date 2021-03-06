warning_flags = -Wall -Wextra -Wformat=2 -Winit-self -Wshadow\
		-Wpointer-arith -Wcast-qual -Wcast-align\
		-Wlogical-op -Wredundant-decls\
		-Wswitch-enum\
		-pedantic\
		-fno-nonansi-builtins -Wundef
#		-Wzero-as-null-pointer-constant

release_flags = -DNDEBUG

test_flags = -D_FORTIFY_SOURCE=2 -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC\
		-D_GLIBCXX_CONCEPT_CHECKS
#	-g
#	-Wconversion 
