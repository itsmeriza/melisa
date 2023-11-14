unit Commons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring

  commands  = '   help    : list all commands.' + sLineBreak +
              '   start   : start the server.' + sLineBreak +
              '   stop    : stop the server.' + sLineBreak +
              '   restart : restart the server.' + sLineBreak +
              '   info    : view server info.' + sLineBreak +
              '   close   : close this server.' + sLineBreak +
              '   clear   : clear screen console.';
  prompt    = ' !';

  cmdHelp = 'help';
  cmdStart = 'start';
  cmdStop = 'stop';
  cmdClose = 'close';
  cmdClear = 'clear';
  cmdRestart = 'restart';
  cmdInfo = 'info';

  MSG_BEWARE  = 'Melisa server. Do not close this window or server will shutdown.';
  MSG_SERVER_STOPPED = 'Server stopped.';

  DAEMONIZED_OPT = '-d';
  RUN_OPT = '-r';

const
  COMMANDS_FILE = 'Commands.dat';

  //COMMAND_LIST =  'GET /1.0/user/get/' +
  //                //',GET /1.0/user/get-token/' +
  //                ',GET /1.0/user/check-block-status/' +
  //                ',GET /1.0/user/get-list/' +
  //                ',GET /1.0/user/is-exist/' +
  //                ',POST /1.0/user/logon/' +
  //                ',POST /1.0/user/logoff/' +
  //                ',POST /1.0/user/add/' +
  //                ',POST /1.0/user/update/'  +
  //                ',POST /1.0/user/delete/'  +
  //                ',POST /1.0/user/change-password/' +
  //
  //                ',GET /1.0/employee/is-exist/' +
  //                ',GET /1.0/employee/get-name/' +
  //
  //                ',GET /1.0/group/get-list-pair/' +
  //                ',GET /1.0/group/get-list/' +
  //                ',GET /1.0/group/get/' +
  //                ',GET /1.0/group/get-access-rights/' +
  //                ',POST /1.0/group/update/' +
  //                ',POST /1.0/group/add/' +
  //                ',POST /1.0/group/delete/' +
  //                ',POST /1.0/group/update-access-rights/' +
  //
  //                ',GET /1.0/access-right/get-forms/' +
  //                ',GET /1.0/access-right/get-access-rights/' +
  //
  //                ',GET /1.0/drq/get-list-template/' +
  //                ',GET /1.0/drq/get-wheres/' +
  //                ',GET /1.0/drq/get-template/' +
  //                ',GET /1.0/drq/get-where/' +
  //                ',GET /1.0/drq/get-templates/' +
  //                ',GET /1.0/drq/get-last-used/' +
  //                ',GET /1.0/drq/is-exist-template/' +
  //                ',GET /1.0/drq/is-exist-where/' +
  //                ',POST /1.0/drq/update-template/' +
  //                ',POST /1.0/drq/add-template/' +
  //                ',POST /1.0/drq/delete-template/' +
  //                ',POST /1.0/drq/add-where/' +
  //                ',POST /1.0/drq/update-where/' +
  //                ',POST /1.0/drq/delete-where/' +
  //                ',POST /1.0/drq/update-last-used/' +
  //
  //                ',GET /1.0/fxkey/get-list/' +
  //                ',GET /1.0/fxkey/get/' +
  //                ',GET /1.0/fxkey/get-last-prefixes/' +
  //                ',GET /1.0/fxkey/is-exist/' +
  //                ',POST /1.0/fxkey/update/' +
  //                ',POST /1.0/fxkey/add/' +
  //                ',POST /1.0/fxkey/delete/' +
  //                ',POST /1.0/fxkey/reset/' +
  //
  //                ',GET /1.0/workplace/get-list-pair/' +
  //                ',GET /1.0/workplace/get-list/' +
  //                ',GET /1.0/workplace/get/' +
  //                ',GET /1.0/workplace/get-types/' +
  //                ',GET /1.0/workplace/is-exist/' +
  //                ',POST /1.0/workplace/update/' +
  //                ',POST /1.0/workplace/add/' +
  //                ',POST /1.0/workplace/delete/' +
  //
  //                ',GET /1.0/uom/get-list/' +
  //                ',GET /1.0/uom/get/' +
  //                ',GET /1.0/uom/is-exist/' +
  //                ',GET /1.0/uom/get-list-pair/' +
  //                ',GET /1.0/uom/get-list-pair-class/' +
  //                ',POST /1.0/uom/add/' +
  //                ',POST /1.0/uom/update/' +
  //                ',POST /1.0/uom/delete/' +
  //                ',GET /1.0/uom/get-list-conversion/' +
  //                ',GET /1.0/uom/get-conversion/' +
  //                ',GET /1.0/uom/get-conversion-value/' +
  //                ',GET /1.0/uom/is-exist-onversion/' +
  //                ',POST /1.0/uom/add-conversion/' +
  //                ',POST /1.0/uom/update-conversion/' +
  //                ',POST /1.0/uom/delete-conversion/' +
  //
  //                ',GET /1.0/moy/get-list-pair/' +
  //                ',GET /1.0/moy/get/' +
  //
  //                ',GET /1.0/config/get-configs/'+
  //                ',GET /1.0/config/get-list/' +
  //                ',GET /1.0/config/get/' +
  //                ',GET /1.0/config/get-value/' +
  //                ',GET /1.0/config/get-classes/' +
  //                ',GET /1.0/config/is-exist/' +
  //                ',GET /1.0/config/get-data-types/' +
  //                ',POST /1.0/config/update/' +
  //                ',POST /1.0/config/add/' +
  //                ',POST /1.0/config/delete/'+
  //
  //                ',GET /1.0/tips/get/' +
  //                ',GET /1.0/tips/get-list/' +
  //
  //                ',GET /1.0/policy/get-all/' +
  //
  //                ',GET /1.0/test/do-test/'
  //                ;

  SQL_CONNECTION_DRIVER_NAME_INITIAL = 'MySQL';
  MQ_CONNECTION_DRIVER_NAME_INITIAL = 'RabbitMQ';
  CNX_1 = 'CNX_1';
  CNX_2 = 'CNX_2';

  STOMP_PORT_DEFAULT = 61613;

{$IFDEF DEBUG}
  DB_VHOST = '192.168.28.3';
  DB_HOST = '192.168.28.3';
{$ENDIF}

  MYSQL_DATETIME_FORMAT = 'yyyy-mm-dd hh:nn:ss';
  MYSQL_DATE_FORME = 'yyyy-mm-dd';

  USER_ACCESS_KEY = 'UserAccess';
  PASSWORD_SALT = 'EtayyoEtayyoo!';
  PASSWORD_KEY = 'MysecretkeyforThepasswd.';

  SQL_PARAM_LENGTH = 4;
  PARAM_REPLACEMENT_CHAR = '?';

implementation

end.


