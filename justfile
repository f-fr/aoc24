build:
	fppkg compile -c debug

release:
	fppkg compile -c release

llvm:
	fppkg-llvm compile

clean:
    fppkg clean
    rm -rf fpmake
