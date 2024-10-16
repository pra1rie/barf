all:
	mlton -runtime stop -codegen native barf.sml

clean:
	rm barf
