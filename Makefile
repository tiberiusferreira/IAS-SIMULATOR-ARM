all: ra139187.o
	gcc -g ra139187.o -o ra139187
ra139187.o: t2ra139187.s
	as -g t2ra139187.s -o ra139187.o
execute:
	@echo '************  EXECUTING FILE  ***********'
	./ra139187 < sum.hex
	@echo '************  END OF EXECUTION  ***********'
clean:
	rm ra139187 ra139187.o t2ra139187.s