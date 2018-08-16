FROM haskell:8.0.2
LABEL application="primo-endpoint" url="http://github.com/NYULibraries/primo-endpoint"
EXPOSE 8080
RUN apt-get update && \
    apt-get install -y libicu-dev && \
    apt-get autoremove --purge -y && \
    apt-get autoclean -y && \
    rm -rf  /var/lib/apt/lists/*

VOLUME /cache
WORKDIR /app
COPY . /app
RUN stack install --system-ghc && \
  rm -rf .stack-work ~/.stack

ENTRYPOINT ["/root/.local/bin/primo-endpoint", "-C", "/cache", "-w8080"]
CMD ["-l", "-v"]
