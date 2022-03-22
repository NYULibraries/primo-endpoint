FROM haskell:latest
LABEL application="primo-endpoint" url="http://github.com/NYULibraries/primo-endpoint"
EXPOSE 8080

WORKDIR /app
COPY . /app

RUN stack install --system-ghc && \
    rm -rf .stack-work ~/.stack && \
    chgrp -R 0 /app && \
    chmod -R g=u /app

USER 1001

ENV PATH=$PATH:/app/.local/bin
ENTRYPOINT ["/app/entrypoint.sh", "-w8080"]
CMD ["-l", "-v"]
