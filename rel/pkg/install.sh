#!/usr/bin/bash

AWK=/usr/bin/awk
SED=/usr/bin/sed

USER=dalmatiner
GROUP=$USER

case $2 in
    PRE-INSTALL)
        if grep "^$GROUP:" /etc/group > /dev/null 2>&1
        then
            echo "Group already exists, skipping creation."
        else
            echo Creating dalmatinerfe group ...
            groupadd $GROUP
        fi
        if id $USER > /dev/null 2>&1
        then
            echo "User already exists, skipping creation."
        else
            echo Creating dalmatinerfe user ...
            useradd -g $GROUP -d /data/ddbpg_autopublisher -s /bin/false $USER
            /usr/sbin/usermod -K defaultpriv=basic,net_privaddr $USER
        fi
        echo Creating directories ...
        mkdir -p /data/ddbpg_autopublisher/etc
        mkdir -p /data/ddbpg_autopublisher/db
        mkdir -p /data/ddbpg_autopublisher/log/sasl
        chown -R $USER:$GROUP /data/ddbpg_autopublisher
        if [ -d /tmp/ddbpg_autopublisher ]
        then
            chown -R $USER:$GROUP /tmp/ddbpg_autopublisher
        fi
        ;;
    POST-INSTALL)
        echo Importing service ...
        svccfg import /opt/local/ddbpg_autopublisher/share/ddbpg_autopublisher.xml
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | $AWK '{print $2}'`

        CONFFILE=/data/ddbpg_autopublisher/etc/ddbpg_autopublisher.conf
        cp /opt/local/ddbpg_autopublisher/etc/ddbpg_autopublisher.conf.example ${CONFFILE}.example

        if [ ! -f "${CONFFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${CONFFILE}.example ${CONFFILE}
            $SED -i bak -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
        else
            echo "Please make sure you update your config according to the update manual!"
        fi
        ;;
esac
