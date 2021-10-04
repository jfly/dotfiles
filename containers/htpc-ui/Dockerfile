FROM umputun/nginx-le:latest

# Copy fix from https://github.com/nginx-le/nginx-le/pull/57.
COPY ./le.sh /le.sh

COPY ./autogen-lefqdn-entrypoint.sh /autogen-lefqdn-entrypoint.sh

ENTRYPOINT ["./autogen-lefqdn-entrypoint.sh"]
