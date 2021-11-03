#!/bin/sh

case "$1" in
    button/lid)
        case "$3" in
            close)
                if nmcli -t connection show --active | cut -d : -f 1 | grep "^\(Honor\|Honor Ethernet\|Cal 3\|Cal 3.5\|Cal\|Hen Wen\|Auto Ethernet\)$" &> /dev/null; then
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
