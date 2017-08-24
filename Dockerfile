FROM fedora:25

MAINTAINER Lubomír Sedlář <lubomir.sedlar@gmail.com>

RUN dnf install -y postgresql-devel zeromq-devel zlib-devel xz make perl && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    dnf clean all && \
    useradd stack

USER stack
ENV PATH=/root/.local/bin:/home/stack/.local/bin:$PATH

RUN mkdir /home/stack/proj
COPY stack.yaml /home/stack/proj/stack.yaml
COPY talk-compose.cabal /home/stack/proj/talk-compose.cabal
RUN cd /home/stack/proj && \
    stack setup --install-ghc && \
    stack build --only-dependencies --dry-run --prefetch

USER root
CMD ["bash"]
