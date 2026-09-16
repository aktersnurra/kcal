#!/bin/sh
# PROVIDE: kcal
# REQUIRE: NETWORKING
# KEYWORD: shutdown
. /etc/rc.subr
name=kcal
rcvar=kcal_enable
load_rc_config "$name"
: ${kcal_enable:=NO}
: ${kcal_user:=kcal}
: ${kcal_env:=/usr/local/etc/kcal.env}
: ${kcal_command:=/usr/local/bin/kcal}
command="/usr/sbin/daemon"
command_args="-f -u ${kcal_user} -o /var/log/kcal.log ${kcal_command} serve"
start_precmd="kcal_prestart"
kcal_prestart() { test -r "${kcal_env}" && . "${kcal_env}"; export KCAL_DATABASE_PATH KCAL_LISTEN_ADDRESS KCAL_PUBLIC_BASE_URL KCAL_OIDC_ISSUER KCAL_OIDC_AUDIENCE; }
run_rc_command "$1"
