FROM haskell:8.0.2
LABEL application="primo-endpoint" url="http://github.com/NYULibraries/primo-endpoint"
EXPOSE 8080
RUN apt-get update && \
    apt-get install -y libicu-dev && \
    apt-get autoremove --purge -y && \
    apt-get autoclean -y && \
    rm -rf /var/cache/apt/* /var/lib/apt/lists/*

VOLUME /cache
WORKDIR /app
COPY . /app
RUN stack install --system-ghc && \
  rm -rf .stack-work ~/.stack

RUN  touch auth.yml && \ 
     echo "archive.nyu.edu" >>auth.yml && \
     echo " headers:">>auth.yml && \
     echo "  rest-dspace-token: ENV[$TOKEN_FDA]" >auth.yml

ENTRYPOINT ["/root/.local/bin/primo-endpoint", "-C", "/cache", "-w8080" "-a auth.yml"]
CMD ["-l", "-v"]
