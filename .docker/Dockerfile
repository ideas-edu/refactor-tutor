FROM rpt-builder as builder
WORKDIR /home/user

# First build to cache deps
RUN bash -ic "stack new rpt; exit"
WORKDIR /home/user/rpt
RUN rm package.yaml; \
    mkdir -p src/Test; \
    echo 'main = putStrLn "Hello, world!"' > src/Main.hs; \
    echo 'main = putStrLn "Hello, world!"' > src/Test/IPTTests.hs;

COPY --chown=user rpt.cabal stack.yaml ./
RUN bash -ic "stack build; exit"

COPY --chown=user ./src ./src
RUN bash -ic "stack build; exit"

USER root
FROM httpd:bullseye as runner
RUN apt update; apt install -y libsqlite3-dev zlib1g-dev
COPY .docker/forward_env_start_httpd /usr/local/apache2/
RUN rm -rf /usr/local/apache2/htdocs/*
COPY ./www /usr/local/apache2/htdocs
COPY --from=builder /home/user/rpt/.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/rpt/rpt /usr/local/apache2/cgi-bin/rpt
COPY  ./scripts/ref.txt /usr/local/apache2/cgi-bin/ref.txt
#RUN sed -i 's/#LoadModule cgi_module/LoadModule cgi_module/' /usr/local/apache2/conf/httpd.conf
#RUN sed -i 's/#LoadModule cgid_module/LoadModule cgid_module/' /usr/local/apache2/conf/httpd.conf
#RUN sed -i 's/DirectoryIndex index.html/DirectoryIndex index.sh index.html/' /usr/local/apache2/conf/httpd.conf
#RUN sed -i 's/#AddHandler cgi-script .cgi/AddHandler cgi-script .sh/' /usr/local/apache2/conf/httpd.conf
COPY ./.docker/httpd.conf /usr/local/apache2/conf/httpd.conf 
RUN chown -R daemon /usr/local/apache2/cgi-bin
ENTRYPOINT ["/usr/local/apache2/forward_env_start_httpd"]