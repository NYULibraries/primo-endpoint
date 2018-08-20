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
    rm -rf .stack-work ~/.stack && \
    chmod 775 /app/entrypoint.sh

ENTRYPOINT ["/app/entrypoint.sh", "-C", "/cache", "-w8080", "-a", "/app/auth.yml" ]
CMD [ "-l", "-v"]
