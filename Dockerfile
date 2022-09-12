FROM racket/racket:8.6-full

RUN raco pkg install --auto rosette fmt

RUN apt-get install -y libgmp3-dev python3-pip git cmake ant libboost-all-dev openjdk-11-jdk python3.6 ninja-build

RUN cd /usr/local/lib && \
    curl -O https://www.antlr.org/download/antlr-4.10.1-complete.jar

RUN  git clone https://github.com/Veridise/pip-cairo-lang.git && \
     cd pip-cairo-lang && \
     pip install . && \
     cd .. && \
     rm -rf pip-cairo-lang

RUN git clone https://github.com/Veridise/V.git && \
    cd V && \
    mkdir -p build && \
    cd build && \
    cmake -GNinja .. && \
    cmake --build .

CMD [ "/bin/bash" ]
