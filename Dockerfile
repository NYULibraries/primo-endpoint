FROM haskell:8.0.2
LABEL application="primo-endpoint" url="http://github.com/NYULibraries/primo-endpoint"
EXPOSE 8080
RUN apt-get update && \
    apt-get install -y libicu-dev && \
    apt-get autoremove --purge -y && \
    apt-get autoclean -y && \
    rm -rf /var/cache/apt/* /var/lib/apt/lists/*

VOLUME /cache
RUN useradd -d /app -m app
WORKDIR /app
COPY --chown=app . /app
USER app

RUN stack install --system-ghc && \
    rm -rf .stack-work ~/.stack

<<<<<<< HEAD
RUN  touch auth.yml && \ 
     echo "archive.nyu.edu" >>auth.yml && \
     echo " headers:">>auth.yml && \
     echo "  rest-dspace-token: ENV[$TOKEN_FDA]" >>auth.yml

ENTRYPOINT ["/entrypoint.sh"]
CMD ["-C", "/cache", "-w8080" "-a auth.yml" "-l", "-v"]
=======
ENV PATH=$PATH:/app/.local/bin
ENTRYPOINT ["/app/entrypoint.sh", "-C", "/cache", "-w8080"]
CMD ["-l", "-v"]
>>>>>>> b6b0d9119664e9de079e86cfc9cb9d4faa645799
