#!/bin/sh

case "$1" in
    button/lid)
        case "$3" in
            close)
                connection=$(nmcli -t connection show --active | cut -d : -f 1)
                if [ "$connection" == "dagron" ]; then
                    logger "Ignoring lid close event because we're connected to: $connection"
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
