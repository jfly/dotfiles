#!/bin/sh

case "$1" in
    button/lid)
        case "$3" in
            close)
                if nmcli -t connection show --active | cut -d : -f 1 | grep "^\(Honor\|dagron\|Honor Ethernet\|NSA Surveillance Van 37\|NSA Surveillance Van 37-2.4G\|Cal 3\|Cal 3.5\)$" &> /dev/null; then
                    logger "Ignoring lid close event because we're connected to: $(nmcli -t connection show --active | cut -d : -f 1)"
                else
                    slock
                fi
                ;;
            open)
                ;;
            *)
                logger "ACPI action undefined: $3"
                ;;
    esac
    ;;
    *)
        logger "ACPI group/action undefined: $1 / $2"
        ;;
esac
