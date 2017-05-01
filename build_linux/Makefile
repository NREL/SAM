all:
	cd ../lhs/src && make 
	cd ../stepwise && make 
	cd ../lk/build_linux && make -j4
	cd ../wex/build_linux && make -j4
	cd ../ssc/build_linux && make -j4
	cd ../SAMnt/build_linux && make -j4
	cd ../SAMnt/build_linux && make package

clean:
	cd ../lhs/src && make clean
	cd ../stepwise && make clean
	cd ../lk/build_linux && make clean
	cd ../wex/build_linux && make clean
	cd ../ssc/build_linux && make clean
	cd ../SAMnt/build_linux && make clean
