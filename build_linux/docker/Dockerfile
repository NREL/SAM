FROM centos:7

RUN yum -y update && yum clean all
RUN yum -y install git gcc make zlib-devel bzip2 bzip2-devel readline-devel \
	    sqlite sqlite-devel openssl-devel tk-devel libffi-devel tmux vim \
	    unzip wget gcc-c++ gtk+-devel gtk2-devel mesa-libGLU-devel \
	    mesa-libGL-devel libcurl-devel

# Download binaries for CMake.
WORKDIR /opt
RUN wget https://github.com/Kitware/CMake/releases/download/v3.16.0/cmake-3.16.0-Linux-x86_64.tar.gz
RUN tar -xzf cmake-3.16.0-Linux-x86_64.tar.gz
RUN rm -f cmake-3.16.0-Linux-x86_64.tar.gz
ENV PATH /opt/cmake-3.16.0-Linux-x86_64/bin:$PATH

# Download and build wxWidgets.
WORKDIR /opt
RUN wget https://github.com/wxWidgets/wxWidgets/releases/download/v3.1.1/wxWidgets-3.1.1.tar.bz2
RUN tar -xjf wxWidgets-3.1.1.tar.bz2
RUN rm -f /opt/wxWidgets-3.1.1.tar.bz2
WORKDIR /opt/wxWidgets-3.1.1/release-build
RUN ../configure --prefix=/opt/wxWidgets-3.1.1/release-build --enable-shared=no --enable-stl=yes \
			  --enable-debug=no --with-gtk=2 --with-libjpeg=builtin --with-libpng=builtin --with-regex=builtin \
			  --with-libtiff=builtin --with-zlib=builtin --with-expat=builtin --without-libjbig \
			  --without-liblzma --without-gtkprint --with-libnotify=no --with-libmspack=no --with-gnomevfs=no \
			  --with-opengl=yes --with-sdl=no --with-cxx=11 --enable-webview
RUN make -j$(nproc)
RUN ln -s /opt/wxWidgets-3.1.1/release-build/wx-config /usr/local/bin/wx-config-3

# Download and build googletest.
WORKDIR /opt
RUN wget https://github.com/google/googletest/archive/master.zip
RUN unzip master.zip
RUN mv googletest-master googletest
RUN rm -f master.zip
WORKDIR /opt/googletest/build
RUN cmake .. -DCMAKE_BUILD_TYPE=Release
RUN make -j$(nproc)

# Download and build SAM.
RUN mkdir -p /root/sandboxes
WORKDIR /root/sandboxes/sam_dev

RUN git clone -b install-python --single-branch https://github.com/daniel-thom/SAM.git sam
RUN cp sam/build_linux/docker/CMakeLists.txt .
RUN git clone -b fix-linux-build --single-branch https://github.com/daniel-thom/ssc.git ssc
RUN git clone https://github.com/NREL/wex.git wex
RUN git clone https://github.com/NREL/lk.git lk

ENV LKDIR /root/sandboxes/sam_dev/lk
ENV WEXDIR /root/sandboxes/sam_dev/wex
ENV SSCDIR /root/sandboxes/sam_dev/ssc
ENV SAMNTDIR /root/sandboxes/sam_dev/sam
ENV GTEST /opt/googletest

WORKDIR /root/sandboxes/sam_dev/build
RUN cmake .. -DCMAKE_BUILD_TYPE=Release
RUN make -j$(nproc)
