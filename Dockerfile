FROM haskell:8.0.2
LABEL application="primo-endpoint" url="http://github.com/NYULibraries/primo-endpoint"
EXPOSE 80
RUN apt-get update
RUN apt-get install -y libicu-dev
#RUN stack config set system-ghc --global true

VOLUME /cache
WORKDIR /app
COPY stack.yaml primo-endpoint.cabal /app/
RUN stack install --only-dependencies
COPY . /app
RUN stack install

ENTRYPOINT ["/root/.local/bin/primo-endpoint", "-C", "/cache", "-w"]
CMD ["-l", "-v"]
