FROM ubuntu:20.04 AS builder
RUN apt update; apt install -y sudo
RUN useradd -u 1000 -m user; \
    usermod -aG sudo user
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER user
ENV USER=user
ENV SHELL=/bin/bash
COPY .docker/setup.sh /tmp
RUN /tmp/setup.sh
