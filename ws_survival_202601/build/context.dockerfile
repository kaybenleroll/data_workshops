FROM busybox

RUN mkdir /tmp/build/
# Add context to /tmp/build/
COPY . /tmp/build/
