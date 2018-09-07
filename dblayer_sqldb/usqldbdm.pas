{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 15.07.2015
*******************************************************************************}
unit uSQLDBDM;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, uBaseDatasetInterfaces,syncobjs,dateutils,
  uAbstractDBLayer,sqldb;
type
  TUnprotectedDataSet = class(TDataSet);
  { TSQLConnection }

  TSQLConnection = class(TSQLConnector,IBaseDBConnection)
    procedure SQLConnectionLog(Sender : TSQLConnection; EventType : TDBEventType; Const Msg : String);
  private
    FProperties: String;
    FEData: Boolean;
    FDatabaseDir: String;
    FLimitAfterSelect: Boolean;
    FLimitSTMT: String;
    FInTransaction : Boolean;
    FDBTyp: String;
    function DoExecuteDirect(aSQL: string): Integer;
    function DoGetTableNames(aTables: TStrings): Boolean;
    function DoGetTriggerNames(aTriggers: TStrings): Boolean;
    function DoGetColumns(aTableName: string): TStrings;
    function DoGetIndexes(aTableName: string): TStrings;
    function DoSetProperties(aProp : string) : Boolean;
    function DoCreateDBFromProperties(aProp : string) : Boolean;
    function DoInitializeConnection: Boolean;
    function DoStartTransaction(ForceTransaction : Boolean = False): Boolean;
    function DoCommitTransaction: Boolean;
    function DoRollbackTransaction: Boolean;
    function IsTransactionActive: Boolean;
    procedure DoDisconnect;
    procedure DoConnect;
    function GetHandle : Pointer;
    function GetDatabaseName: string;
    function GetUniID(aConnection: TComponent; Generator: string;
      Tablename: string; AutoInc: Boolean): Variant;
    function IsConnected: Boolean;
    function GetLimitAfterSelect: Boolean;
    function GetLimitSTMT: string;
    function DoGetDBLayerType: string;
    function GetSyncOffset: Integer;
    procedure SetSyncOffset(const AValue: Integer);
    function UseExtData: Boolean;
    function GetDatabaseDir: string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TSQLDBDataSet }

  TSQLDBDataSet = class(TSQLQuery,IBaseDBFilter,IBaseManageDB,IBaseSubDatasets,IBaseModifiedDS)
    procedure TDateTimeFieldGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
  private
    FFirstOpen : Boolean;
    FSubDataSets : Tlist;
    FFields : string;
    FFilter,FBaseFilter,FIntFilter : string;
    FLimit : Integer;
    FMDS: TDataSource;
    FSortDirection : TSortDirection;
    FSortFields : string;
    FTableNames : string;
    FDefaultTableName : string;
    FManagedFieldDefs : TFieldDefs;
    FManagedIndexDefs : TIndexDefs;
    FOrigTable : TAbstractDBDataset;
    FUsePermissions : Boolean;
    FTableCaption : string;
    FDistinct : Boolean;
    FUpStdFields : Boolean;
    FUpChangedBy : Boolean;
    FBaseSortFields : string;
    FBaseSorting : string;
    FBaseSortDirection : TSortDirection;
    FUseBaseSorting : Boolean;
    FUseIntegrity : Boolean;
    FChangeUni : Boolean;
    FSQL,FIntSQL : string;
    FParams : TStringList;
    FInBeforePost : Boolean;
    FHasNewID : Boolean;
    procedure SetNewIDIfNull;
    function IndexExists(aIndexName : string) : Boolean;
    procedure WaitForLostConnection;
    procedure DoUpdateSQL;
  protected
    //Internal DataSet Methods that needs to be changed
    procedure InternalOpen; override;
    procedure InternalRefresh; override;
    procedure InternalPost; override;
    procedure DoAfterInsert; override;
    procedure DoBeforePost; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeEdit; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure DoBeforeDelete; override;
    procedure DoAfterDelete; override;
    procedure DoAfterScroll; override;
    procedure DoBeforeCancel; override;
    //IBaseDBFilter
    function GetFields: string;
    function GetBaseFilter: string;
    function GetLimit: Integer;
    function GetSortDirection: TSortDirection;
    function GetSortFields: string;
    function GetLocalSortFields : string;
    function GetBaseSortFields: string;
    function GetSortLocal: Boolean;
    procedure SetFields(const AValue: string);
    function GetFilter: string;
    function GetIntFilter: string;
    procedure SetFilter(const AValue: string);
    procedure SetBaseFilter(const AValue: string);
    function GetSQL: string;
    procedure SetSQL(const AValue: string);
    procedure Setlimit(const AValue: Integer);
    procedure SetSortDirection(const AValue: TSortDirection);
    procedure SetSortFields(const AValue: string);
    procedure SetLocalSortFields(const AValue : string);
    procedure SetBaseSortFields(const AValue: string);
    procedure SetSortLocal(const AValue: Boolean);
    function GetFilterTables: string;
    procedure SetFilterTables(const AValue: string);
    function GetUsePermissions: Boolean;
    procedure SetUsePermisions(const AValue: Boolean);
    function GetDistinct: Boolean;
    procedure SetDistinct(const AValue: Boolean);
    function GetBaseSorting: string;
    procedure SetBaseSorting(AValue: string);
    function GetBaseSortDirection: TSortDirection;
    procedure SetBaseSortDirection(AValue: TSortDirection);
    function GetUseBaseSorting: Boolean;
    procedure SetUseBaseSorting(AValue: Boolean);
    function GetfetchRows: Integer;
    procedure SetfetchRows(AValue: Integer);
    function GetParameterValue(const aName: string): Variant;
    procedure SetParameterValue(const aName: string; AValue: Variant);
    //IBaseManageDB
    function GetManagedFieldDefs: TFieldDefs;
    function GetManagedIndexDefs: TIndexDefs;
    function GetTableName: string;
    procedure SetTableName(const AValue: string);
    function GetTableNames: string;
    function GetConnection: TComponent;
    function GetTableCaption: string;
    procedure SetTableCaption(const AValue: string);
    function GetUpStdFields: Boolean;
    procedure SetUpStdFields(AValue: Boolean);
    function GetUpChangedBy: Boolean;
    procedure SetUpChangedBy(AValue: Boolean);
    function GetUseIntegrity: Boolean;
    procedure SetUseIntegrity(AValue: Boolean);
    function GetAsReadonly: Boolean;
    procedure SetAsReadonly(AValue: Boolean);
    procedure SetConnection(aConn: TComponent);
    function GetMasterdataSource: TDataSource;
    procedure SetMasterdataSource(AValue: TDataSource);
    procedure SetTableNames(const AValue: string);
    procedure SetOrigTable(AValue: TComponent);
    function GetOrigTable : TComponent;
    procedure SetDataSource(AValue: TDataSource);
    //IBaseSubDataSets
    function GetSubDataSet(aName : string): TComponent;
    procedure RegisterSubDataSet(aDataSet : TComponent);
    function GetCount : Integer;
    function GetSubDataSetIdx(aIdx : Integer): TComponent;
    //IBaseModifiedDS
    function IsChanged: Boolean;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    property MasterDataSource : TDataSource read FMDS write FMDS;
    property DefaultTableName : string read FDefaultTableName;
    procedure DoExecSQL;
    function NumRowsAffected: Integer;
    procedure Post; override;
  end;
implementation
uses uEncrypt,mssqlconn, sqlite3conn, sqlite3dyn, pqconnection;
resourcestring
  strUnknownDbType                = 'Unbekannter Datenbanktyp';
  strDatabaseConnectionLost       = 'Die Datenbankverbindung wurde verloren !';

{ TSQLConnection }

function TSQLConnection.DoSetProperties(aProp: string): Boolean;
var
  tmp: String;
  actDir: String;
  actVer: LongInt;
  sl: TStringList;
begin
//  if Assigned(BaseApplication) then
//    with BaseApplication as IBaseDBInterface do
//      LastError := '';
  FProperties := aProp;
  Result := True;
  tmp := aProp;
  try
    //CleanupSession;
    if Connected then
      Connected := False;
    Port:=0;
    Params.Clear;
    Params.Add('timeout=3');
    ConnectorType:='';
    UserName:='';
    Password:='';
    HostName:='';
    DatabaseName:='';
    if copy(tmp,0,pos(';',tmp)-1) <> 'sqlite-3-edata' then
      ConnectorType:=copy(tmp,0,pos(';',tmp)-1)
    else
      begin
        ConnectorType:='sqlite-3';
        FEData:=True;
      end;
    if ConnectorType = 'mssql' then
      ConnectorType := 'MSSQLServer'
    else if copy(ConnectorType,0,8) = 'postgres' then
      ConnectorType := 'PostgreSQL'
    else if (copy(ConnectorType,0,6) = 'sqlite') then
      ConnectorType := 'SQLite3';
    Assert(ConnectorType<>'',strUnknownDbType);
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    HostName := copy(tmp,0,pos(';',tmp)-1);
    if pos(':',HostName) > 0 then
      begin
        Port:=StrToInt(copy(HostName,pos(':',HostName)+1,length(HostName)));
        HostName:=copy(HostName,0,pos(':',HostName)-1);
      end
    else if pos('/',HostName) > 0 then
      begin
        Port:=StrToInt(copy(HostName,pos('/',HostName)+1,length(HostName)));
        HostName:=copy(HostName,0,pos('/',HostName)-1);
      end;
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    DatabaseName:=copy(tmp,0,pos(';',tmp)-1);
    FDatabaseDir:=ExtractFileDir(ExpandFileName(DatabaseName));
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    UserName := copy(tmp,0,pos(';',tmp)-1);
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    if copy(tmp,0,1) = 'x' then
      Password := Decrypt(copy(tmp,2,length(tmp)),99998)
    else
      Password := tmp;
    if (copy(ConnectorType,0,6) = 'sqlite')
    then
      begin
//        if Connection=MainConnection then //Dont check this on attatched db´s (we want to create them on the fly)
//          if not FileExists(Database) then
//            raise Exception.Create('Databasefile dosend exists');
        //TransactIsolationLevel:=tiNone;
        sqlite3_busy_timeout(TSQLite3Connection(Proxy).Handle,10000);
      end;
    if (copy(ConnectorType,0,8) = 'postgres')
    then
      begin
        Params.Add('compression=true');
        {$IFDEF CPUARM}
        Properties.Add('sslmode=disable');
        {$ENDIF}
        //TransactIsolationLevel:=tiNone;
        //AutoEncodeStrings:=true;
      end
    else if (copy(ConnectorType,0,8) = 'firebird')
    or (copy(ConnectorType,0,9) = 'interbase')
    then
      begin
        //TransactIsolationLevel:=tiReadCommitted;
      end
    else if (copy(ConnectorType,0,5) = 'mysql') then
      begin
        //TransactIsolationLevel:=tiReadUncommitted;
        Params.Clear;
        Params.Add('compression=true');
        //Properties.Add('codepage=UTF8');
        //Properties.Add('timeout=0');
        Params.Add('ValidateUpdateCount=-1');
        Params.Add('MYSQL_OPT_RECONNECT=TRUE');
      end
    else if (copy(ConnectorType,0,5) = 'mssql') then
      begin
        //TransactIsolationLevel:=tiReadUncommitted;
        //AutoEncodeStrings:=true;
      end;
    Params.Add('Undefined_Varchar_AsString_Length= 255');
  finally
  end;
end;
function TSQLConnection.DoCreateDBFromProperties(aProp: string): Boolean;
var
  aDatabase: String;
  aUser: String;
  aPassword: String;
begin
  DoSetProperties(aProp);
  aDatabase := DatabaseName;
  aUser := UserName;
  aPassword := Password;
  if (copy(ConnectorType,0,8) = 'postgres')
  then
    begin
      DatabaseName:='postgres';
    end
    else if (copy(ConnectorType,0,5) = 'mssql') then
      Params.Add('CreateNewDatabase=CREATE DATABASE "'+aDatabase+'"')
    else if (copy(ConnectorType,0,8) = 'firebird')
    or (copy(ConnectorType,0,9) = 'interbase')
    then
      begin
        if HostName <> '' then
          Params.Add('CreateNewDatabase=CREATE DATABASE '''+HostName+':'+aDatabase+''' USER '''+aUser+''' PASSWORD '''+aPassword+''' PAGE_SIZE = 4096 DEFAULT CHARACTER SET UTF8')
        else
          Params.Add('CreateNewDatabase=CREATE DATABASE '''+aDatabase+''' USER '''+aUser+''' PASSWORD '''+aPassword+''' PAGE_SIZE = 4096 DEFAULT CHARACTER SET UTF8');
      end
    else if (copy(ConnectorType,0,6) = 'sqlite') then
      begin
        if ExtractFileDir(DatabaseName)<>'' then
          ForceDirectories(ExtractFileDir(DatabaseName));
      end;
    try
      Connected:=True;
    except
      on e : Exception do
//      if Assigned(BaseApplication) then
//        with BaseApplication as IBaseDBInterface do
//          LastError := e.Message;
    end;
    if (copy(ConnectorType,0,8) = 'postgres')
    then
      begin
        //Result :=
        ExecuteDirect('CREATE DATABASE "'+aDatabase+'" WITH OWNER = "'+aUser+'" ENCODING = ''UTF8'' CONNECTION LIMIT = -1;');
        Connected:=False;
        DatabaseName:=aDatabase;
      end;
    Connected:=True;
    Result := Connected;
    Connected:=False;
end;
function TSQLConnection.DoInitializeConnection: Boolean;
begin
  Result := True;
  FLimitAfterSelect := False;
  FDBTyp:=ConnectorType;
  FLimitSTMT := 'LIMIT %s';
  if copy(ConnectorType,0,6) = 'sqlite' then
    begin
      //ExecuteDirect('PRAGMA synchronous = NORMAL;');
      ExecuteDirect('PRAGMA cache_size = 5120;');
      //ExecuteDirect('PRAGMA auto_vacuum = INCREMENTAL;');
      //ExecuteDirect('PRAGMA journal_mode = TRUNCATE;');
      ExecuteDirect('PRAGMA recursive_triggers = ON;');
      ExecuteDirect('PRAGMA foreign_keys = ON;');
      ExecuteDirect('PRAGMA case_sensitive_like = ON;');
      //ExecuteDirect('PRAGMA secure_delete = ON;');
      //ExecuteDirect('PRAGMA incremental_vacuum(50);');
      FDBTyp := 'sqlite';
    end
  else if (copy(TAbstractDBModule(Owner).GetDBType,0,8) = 'firebird')
       or (copy(TAbstractDBModule(Owner).GetDBType,0,9) = 'interbase') then
    begin
      FDBTyp := 'firebird';
      FLimitSTMT := 'ROWS 1 TO %s';
      {
      if not Assigned(Sequence) then
        begin
          Sequence := TZSequence.Create(Owner);
        end;
      }
    end
  else if TAbstractDBModule(Owner).GetDBType = 'mssql' then
    begin
      FLimitAfterSelect := True;
      FLimitSTMT := 'TOP %s';
    end
  else if (copy(TAbstractDBModule(Owner).GetDBType,0,8) = 'postgres') then
    begin
      FDBTyp := 'postgres';

    end;

end;

procedure TSQLConnection.SQLConnectionLog(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: String);
begin
  try
    if pos('--debug-sql',lowercase(cmdline))>0 then
      if Assigned(Sender.Owner) and Assigned(TAbstractDBModule(Sender.Owner).OnLog) then
        case EventType of
        detPrepare:TAbstractDBModule(Sender.Owner).OnLog(Sender.Owner,'Prepare:'+Msg);
        detExecute:TAbstractDBModule(Sender.Owner).OnLog(Sender.Owner,'Execute:'+Msg);
        detFetch:TAbstractDBModule(Sender.Owner).OnLog(Sender.Owner,'Fetch:'+Msg);
        detCommit:TAbstractDBModule(Sender.Owner).OnLog(Sender.Owner,'Commit:'+Msg);
        detRollBack:TAbstractDBModule(Sender.Owner).OnLog(Sender.Owner,'Rollback:'+Msg);
        end;
  except
  end;
end;

function TSQLConnection.DoExecuteDirect(aSQL: string): Integer;
begin
  ExecuteDirect(aSQL);
end;
function TSQLConnection.DoStartTransaction(ForceTransaction: Boolean): Boolean;
begin
  {
  Tag := Integer(TransactIsolationLevel);
  if ForceTransaction and (copy(ConnectorType,0,6) = 'sqlite') then
    TransactIsolationLevel:=tiReadCommitted
  else if (copy(ConnectorType,0,8) = 'postgres') then
    TransactIsolationLevel:=tiReadCommitted
  else if (copy(ConnectorType,0,5) = 'mssql') then
    TransactIsolationLevel:=tiReadUnCommitted;
  }
  Transaction.StartTransaction;
  FInTransaction:=True;
end;
function TSQLConnection.DoCommitTransaction: Boolean;
begin
  Transaction.Commit;
  //if TZTransactIsolationLevel(Tag) <> TransactIsolationLevel then
  //  TransactIsolationLevel := TZTransactIsolationLevel(Tag);
  FInTransaction:=False;
end;
function TSQLConnection.DoRollbackTransaction: Boolean;
begin
  Transaction.Rollback;
  //if TZTransactIsolationLevel(Tag) <> TransactIsolationLevel then
  //  TransactIsolationLevel := TZTransactIsolationLevel(Tag);
  FInTransaction:=False;
end;

function TSQLConnection.IsTransactionActive: Boolean;
begin
  result := FInTransaction;
end;

procedure TSQLConnection.DoDisconnect;
begin
  try
    Connected := False;
  except
  end;
end;
procedure TSQLConnection.DoConnect;
begin
  FInTransaction:=False;
  try
    Connected := True;
  except on e : Exception do
    begin
//      if Assigned(BaseApplication) then
//        with BaseApplication as IBaseDBInterface do
//          LastError := e.Message;
    end;
  end;
end;

function TSQLConnection.GetHandle: Pointer;
begin
  Result := Handle;
end;

function TSQLConnection.GetDatabaseName: string;
begin
  Result := DatabaseName;
end;

function TSQLConnection.GetUniID(aConnection: TComponent; Generator: string;
  Tablename: string; AutoInc: Boolean): Variant;
var
  Resultset: TSQLQuery;
  bConnection: TComponent;
  aId: Int64 = 0;
begin
  with TAbstractDBModule(Owner) do
   begin
      Result := 0;
      try
        while aId=Result do
          begin
            if AutoInc then
              begin
                if (copy(ConnectorType,0,5) = 'mysql') then
                  begin
                    ExecuteDirect('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'='+QuoteField('ID')+'+1;')
                  end
                else
                  begin
                    if LimitAfterSelect then
                      ExecuteDirect('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'=(select '+Format(LimitSTMT,['1'])+' '+QuoteField('ID')+' from '+QuoteField(Generator)+')+1;')
                    else
                      ExecuteDirect('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'=(select '+QuoteField('ID')+' from '+QuoteField(Generator)+' '+Format(LimitSTMT,['1'])+')+1;');
                  end;
              end;
            try
              ResultSet := TSQLQuery.Create(Self);
              ResultSet.DataBase := Self;
              ResultSet.ReadOnly:=True;
              ResultSet.SQL.Text := 'SELECT '+QuoteField('ID')+' FROM '+QuoteField(Generator);
              Resultset.Open;
              if ResultSet.RecordCount>0 then
                Result := ResultSet.FieldByName('ID').AsLargeInt
              else
                begin
                  ExecuteDirect('insert into '+QuoteField(GENERATOR)+' ('+QuoteField('SQL_ID')+','+QuoteField('ID')+') VALUES (1,1000);');
                  Result := 1000;
                  break;
                end;
              try
                if Tablename<>'' then
                  begin
                    ResultSet.Close;
                    ResultSet.SQL.Text := 'SELECT '+QuoteField('SQL_ID')+' FROM '+QuoteField(Tablename)+' WHERE '+QuoteField('SQL_ID')+'='+QuoteValue(Format('%d',[Int64(Result)]));
                    Resultset.Open;
                    if ResultSet.RecordCount>0 then
                      aId := ResultSet.FieldByName('ID').AsLargeInt;
                  end;
              except
              end;
              ResultSet.Free;
            except
            end;
          end;
        except
        end;
    end;
end;

function TSQLConnection.DoGetTableNames(aTables: TStrings): Boolean;
begin
  Result := True;
  GetTableNames(aTables,False);
end;
function TSQLConnection.DoGetTriggerNames(aTriggers: TStrings): Boolean;
begin
  Result := False;
end;

function TSQLConnection.DoGetColumns(aTableName: string): TStrings;
begin
  Result := TStringList.Create;
  GetFieldNames(aTableName,Result);
end;

function TSQLConnection.DoGetIndexes(aTableName: string): TStrings;
begin
  Result := nil;
end;

function TSQLConnection.IsConnected: Boolean;
begin
  Result := Connected;
end;
function TSQLConnection.GetLimitAfterSelect: Boolean;
begin
  Result := FLimitAfterSelect;
end;
function TSQLConnection.GetLimitSTMT: string;
begin
  Result := FLimitSTMT;
end;

function TSQLConnection.DoGetDBLayerType: string;
begin
  if FDBTyp = '' then
    Result := ConnectorType
  else Result := FDBTyp;
  case ConnectorType of
  'SQLite3':Result := 'sqlite';
  'MSSQLServer':Result := 'mssql';
  'PostgreSQL':Result := 'postgres';
  end;
end;

function TSQLConnection.GetSyncOffset: Integer;
var
  ResultSet: TSQLQuery;
  bConnection: TComponent;
begin
  ResultSet := TSQLQuery.Create(Self);
  ResultSet.SQL.Text := 'SELECT '+TAbstractDBModule(Owner).QuoteField('ID')+' FROM '+TAbstractDBModule(Owner).QuoteField('GEN_SQL_ID');
  ResultSet.Open;
  if ResultSet.RecordCount>0 then
    Result := ResultSet.FieldByName('ID').AsLargeInt shr 56
  else Result := 0;
  ResultSet.Free;
end;

procedure TSQLConnection.SetSyncOffset(const AValue: Integer);
var
  aVal: Integer;
begin
  aVal := AValue;
  aVal := aVal shl 56;
  ExecuteDirect('update '+TAbstractDBModule(Owner).QuoteField('GEN_SQL_ID')+' set '+TAbstractDBModule(Owner).QuoteField('ID')+'='+IntToStr(aVal));
end;

function TSQLConnection.UseExtData: Boolean;
begin
  Result := FEData;
end;

function TSQLConnection.GetDatabaseDir: string;
begin
  Result := FDatabaseDir;
end;

constructor TSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Transaction := TSQLTransaction.Create(Self);
  OnLog:=TDBLogNotifyEvent(@Self.SQLConnectionLog);
  LogEvents:=[detExecute];
end;

destructor TSQLConnection.Destroy;
begin
  inherited Destroy;
end;

procedure TSQLDBDataSet.TDateTimeFieldGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if not Sender.IsNull then
    begin
      if trunc(Sender.AsDateTime)=Sender.AsDateTime then
        aText := FormatDateTime(ShortDateFormat,Sender.AsDateTime)
      else
        aText := FormatDateTime(ShortDateFormat+' '+ShortTimeFormat,Sender.AsDateTime);
    end;
end;
procedure TSQLDBDataSet.SetNewIDIfNull;
begin
  if (FieldDefs.IndexOf('AUTO_ID') = -1) and (FieldDefs.IndexOf('SQL_ID') > -1) and  FieldByName('SQL_ID').IsNull then
    begin
      FieldByName('SQL_ID').AsVariant:=TAbstractDBModule(Self.Owner).GetUniID(Self.DataBase,'GEN_SQL_ID',(Self as IBaseManageDB).TableName);
      FHasNewID:=True;
    end
  else if (FieldDefs.IndexOf('SQL_ID') = -1) and (FieldDefs.IndexOf('AUTO_ID') > -1) and FieldByName('AUTO_ID').IsNull then
    begin
      with Self as IBaseManageDB do
        FieldByName('AUTO_ID').AsVariant:=TAbstractDBModule(Self.Owner).GetUniID(Self.DataBase,'GEN_AUTO_ID',TableName);
      FHasNewID:=True;
    end;
end;

function TSQLDBDataSet.IndexExists(aIndexName: string): Boolean;
var
  CustomQuery: TSQLQuery;
begin
  CustomQuery := TSQLQuery.Create(Self);
  CustomQuery.DataBase := DataBase;
  if (copy(TSQLConnector(TAbstractDBModule(Owner).MainConnection).ConnectorType,0,8) = 'firebird')
  or (copy(TSQLConnector(TAbstractDBModule(Owner).MainConnection).ConnectorType,0,9) = 'interbase') then
    begin
      CustomQuery.SQL.Text := 'select rdb$index_name from rdb$indices where rdb$index_name='+TAbstractDBModule(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TSQLConnector(TAbstractDBModule(Owner).MainConnection).ConnectorType,0,6) = 'sqlite') then
    begin
      CustomQuery.SQL.Text := 'select name from SQLITE_MASTER where "TYPE"=''index'' and NAME='+TAbstractDBModule(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TSQLConnector(TAbstractDBModule(Owner).MainConnection).ConnectorType,0,5) = 'mssql') then
    begin
      CustomQuery.SQL.Text := 'select name from dbo.sysindexes where NAME='+TAbstractDBModule(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TSQLConnector(TAbstractDBModule(Owner).MainConnection).ConnectorType,0,8) = 'postgres') then
    begin
      CustomQuery.SQL.Text := 'select * from pg_class where relname='+TAbstractDBModule(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else
    begin
      {
      Metadata := TZSQLMetaData.Create(TZConnection(TAbstractDBModule(Owner).MainConnection));
      MetaData.Connection := Connection;
      MetaData.MetadataType:=mdIndexInfo;
      Metadata.Catalog:=TZConnection(TAbstractDBModule(Owner).MainConnection).Catalog;
      Metadata.TableName:=copy(indexname,0,pos('_',indexname)-1);
      MetaData.Filter:='INDEX_NAME='+TAbstractDBModule(Owner).QuoteValue(indexname);
      MetaData.Filtered:=True;
      MetaData.Active:=True;
      Result := MetaData.RecordCount > 0;
      MetaData.Free;
      }
    end;
  CustomQuery.Free;
end;

procedure TSQLDBDataSet.WaitForLostConnection;
var
  aConnThere: Boolean;
begin
  if not TAbstractDBModule(Owner).Ping(DataBase) then
    begin
      if Assigned(TAbstractDBModule(Owner).OnConnectionLost) then
        TAbstractDBModule(Owner).OnConnectionLost(TAbstractDBModule(Owner));
      aConnThere := False;
      while not aConnThere do
        begin
          if GetCurrentThreadID=MainThreadID then
            begin
              if Assigned(TAbstractDBModule(Owner).OnDisconnectKeepAlive) then
                TAbstractDBModule(Owner).OnDisconnectKeepAlive(TAbstractDBModule(Owner));
            end;
          try
            if TAbstractDBModule(Owner).Ping(DataBase) then
              aConnThere := True;
            sleep(2000);
          except
            sleep(200);
          end;
        end;
      if Assigned(TAbstractDBModule(Owner).OnConnect) then
        TAbstractDBModule(Owner).OnConnect(TAbstractDBModule(Owner));
    end;
end;

procedure TSQLDBDataSet.DoUpdateSQL;
begin
  Close;
  if FSQL<>'' then
    SetSQL(FSQL)
  else
    begin
      SQL.Text:='';
      FIntSQL := '';
      SetFilter(FFilter);
    end;
end;

procedure TSQLDBDataSet.InternalOpen;
var
  a: Integer;
begin
  if (not Assigned(DataBase)) or (not DataBase.Connected) then exit;
  //if TSQLConnector(DataBase).ConnectorType='mysql' then
  //  Properties.Values['ValidateUpdateCount'] := 'False';
  {$if FPC_FULLVERSION>30000}
  if Assigned(FOrigTable) and Assigned(ForigTable.DataModule) then
    TAbstractDBModule(ForigTable.DataModule).LastTime := GetTickCount64;
  {$endif}
  if TAbstractDBModule(Owner).IgnoreOpenRequests then exit;
  if FFirstOpen then
    begin
      FIntSQL := TAbstractDBModule(Owner).BuildSQL(Self);
      SQL.Text := FIntSQL;
      if (FLimit>0) and Assigned(Params.FindParam('Limit')) and ((copy(TSQLConnector(DataBase).ConnectorType,0,8)<>'postgres') or (FLimit>90))  then
        ParamByName('Limit').AsInteger:=FLimit;
      FFirstOpen:=False;
    end;
  inherited InternalOpen;
  try
  if Assigned(FOrigTable)
  and Assigned(ForigTable.DataModule)
  and Assigned(Self)
  then
    begin
      FOrigTable.SetDisplayLabels(Self);
      if FOrigTable.UpdateFloatFields then
        begin
          DisableControls;
          for a := 0 to Fields.Count -1 do
            begin
              if Fields[a] is TFloatField then
                begin
                  if Fields[a].Name = 'WEIGHT' then
                    begin
                      TFloatField(Fields[a]).DisplayFormat := '#,##0.000##';
                      TFloatField(Fields[a]).EditFormat := '0.000##';
                      TFloatField(Fields[a]).Precision:=5;
                    end
                  else
                    begin
                      TFloatField(Fields[a]).DisplayFormat := '#,##0.00##';
                      TFloatField(Fields[a]).EditFormat := '0.00##';
                      TFloatField(Fields[a]).Precision:=5;
                    end;
                end;
              if (Fields[a] is TDateTimeField)
              then
                begin
                  TDateTimeField(Fields[a]).DisplayFormat := ShortDateFormat+' '+ShortTimeFormat;
                  TDateTimeField(Fields[a]).OnGetText:=@TDateTimeFieldGetText;
                end;
            end;
          EnableControls;
        end;
    end;
  except
    begin
      FOrigTable:=nil;
      raise;
    end;
  end;
end;

procedure TSQLDBDataSet.InternalRefresh;
begin
  if TAbstractDBModule(Owner).IgnoreOpenRequests then exit;
//  if Assigned(FOrigTable) and Assigned(ForigTable.DataModule) then
//    TAbstractDBModule(ForigTable.DataModule).CriticalSection.Enter;
  try
  try
    inherited InternalRefresh;
  except
    InternalClose;
    if not Active then
      begin
        if TAbstractDBModule(Owner).Ping(DataBase) then
          InternalOpen
        else
          begin
            WaitForLostConnection;
            InternalOpen;
          end;
      end;
  end;
  finally
//    if Assigned(FOrigTable) and Assigned(ForigTable.DataModule) then
//      TAbstractDBModule(ForigTable.DataModule).CriticalSection.Leave;
  end;
end;

procedure TSQLDBDataSet.InternalPost;
begin
  inherited InternalPost;
end;

procedure TSQLDBDataSet.DoAfterInsert;
begin
  inherited DoAfterInsert;
  if Assigned(FOrigTable) then
    begin
      FOrigTable.DisableChanges;
      FOrigTable.FillDefaults(Self);
      FOrigTable.EnableChanges;
    end;
end;
procedure TSQLDBDataSet.DoBeforePost;
var
  UserCode: String;
begin
  inherited DoBeforePost;
  if FInBeforePost then exit;
  try
  FInBeforePost := True;
  if Assigned(Self.FOrigTable) then
    Self.FOrigTable.DisableChanges;
  FHasNewID:=False;
  SetNewIDIfNull;
  if FUpStdFields and Assigned(FOrigTable) {and (FOrigTable.Changed)} then
    begin
      {$IF FPC_FULLVERSION>20600}
      if (FieldDefs.IndexOf('TIMESTAMPD') > -1) then
        FieldByName('TIMESTAMPD').AsDateTime:=LocalTimeToUniversal(Now());
      {$ELSE}
      if (FieldDefs.IndexOf('TIMESTAMPD') > -1) then
        FieldByName('TIMESTAMPD').AsDateTime:=Now();
      {$ENDIF}
      UserCode := TAbstractDBModule(ForigTable.DataModule).GetUserCode;
      if (FieldDefs.IndexOf('CREATEDBY') > -1) and (FieldByName('CREATEDBY').IsNull) then
        FieldByName('CREATEDBY').AsString:=UserCode;
      if FUpChangedBy and (FieldDefs.IndexOf('CHANGEDBY') > -1) then
        FieldByName('CHANGEDBY').AsString:=UserCode;
    end;
  if Assigned(DataSource) and (FieldDefs.IndexOf('REF_ID')>-1) and  Assigned(FieldByName('REF_ID')) and FieldbyName('REF_ID').IsNull then
    begin
      if DataSource.DataSet.FieldDefs.IndexOf('AUTO_ID') > -1 then
        FieldbyName('REF_ID').AsVariant:=DataSource.DataSet.FieldByName('AUTO_ID').AsVariant
      else
        FieldbyName('REF_ID').AsVariant:=DataSource.DataSet.FieldByName('SQL_ID').AsVariant;
    end;
  finally
    FInBeforePost:= False;
    if Assigned(Self.FOrigTable) then
      Self.FOrigTable.EnableChanges;
  end;
end;
procedure TSQLDBDataSet.DoBeforeInsert;
begin
  if Assigned(DataSource) then
    begin
      if (DataSource.State <> dsInsert) and (DataSource.DataSet.RecordCount = 0) then
        begin
          DataSource.DataSet.Append;
        end;
      if (DataSource.DataSet.State = dsInsert) then
        begin
          DataSource.DataSet.Post;
          DataSource.DataSet.Edit;
        end;
    end;
  inherited DoBeforeInsert;
end;

procedure TSQLDBDataSet.DoBeforeEdit;
begin
  inherited DoBeforeEdit;
end;

procedure TSQLDBDataSet.DoBeforeDelete;
begin
  inherited DoBeforeDelete;
  if (GetTableName='DELETEDITEMS')
  or (GetTableName='DBTABLES')
  then exit;
  try
    if Assigned(FOrigTable) and Assigned(FOrigTable.OnRemove) then FOrigTable.OnRemove(FOrigTable);
//    if GetUpStdFields = True then
//      TAbstractDBModule(Owner).DeleteItem(FOrigTable);
  except
  end;
end;
procedure TSQLDBDataSet.DoAfterDelete;
begin
  inherited DoAfterDelete;
  if Assigned(FOrigTable) then
    FOrigTable.Change;
end;
procedure TSQLDBDataSet.DoAfterScroll;
begin
  inherited DoAfterScroll;
  if Assigned(ForigTable) then
    FOrigTable.UnChange;
end;

procedure TSQLDBDataSet.DoBeforeCancel;
begin
  inherited DoBeforeCancel;
  if State = dsInsert then
    begin
      if Assigned(FOrigTable) and Assigned(FOrigTable.OnRemove) then FOrigTable.OnRemove(FOrigTable);
    end;
end;

function TSQLDBDataSet.GetFields: string;
begin
  Result := FFields;
end;
function TSQLDBDataSet.GetFilter: string;
begin
  Result := FFilter;
end;

function TSQLDBDataSet.GetIntFilter: string;
begin
  Result := FIntFilter;
end;

function TSQLDBDataSet.GetBaseFilter: string;
begin
  Result := FBaseFilter;
end;
function TSQLDBDataSet.GetLimit: Integer;
begin
  Result := FLimit;
end;
function TSQLDBDataSet.GetSortDirection: TSortDirection;
begin
  Result := FSortDirection;
end;
function TSQLDBDataSet.GetSortFields: string;
begin
  Result := FSortFields;
end;

function TSQLDBDataSet.GetLocalSortFields: string;
begin
  Result := '';//SortedFields;
end;

function TSQLDBDataSet.GetBaseSortFields: string;
begin
  Result := FBaseSortFields;
end;
function TSQLDBDataSet.GetSortLocal: Boolean;
begin
  Result := False;//SortType <> stIgnored;
end;
procedure TSQLDBDataSet.SetFields(const AValue: string);
begin
  if FFields<>AValue then
    begin
      FFields := AValue;
      DoUpdateSQL;
    end;
end;
procedure TSQLDBDataSet.SetFilter(const AValue: string);
var
  NewSQL: string;
  i: Integer;
  aPar: TParam;
begin
  if (FFilter=AValue) and (SQL.text<>'')  then
    begin
      if (AValue<>'') or (pos('where',lowercase(SQL.Text))=0) then
        exit;
    end;
  TAbstractDBModule(Owner).ParameteriseSQL:=False;
  TAbstractDBModule(Owner).DecodeFilter(AValue,FParams,NewSQL);
  Close;
  FFilter := AValue;
  if (FIntFilter<>NewSQL) or (SQL.Text='')  then //Params and SQL has changed
    begin
      FSQL := '';
      if TAbstractDBModule(Owner).CheckForInjection(AValue) then exit;
      FIntFilter:=NewSQL;
      FIntSQL := '';
      FIntSQL := TAbstractDBModule(Owner).BuildSQL(Self);
      Params.Clear;
      SQL.text := FIntSQL;
    end;
  for i := 0 to FParams.Count-1 do
    begin
      if Assigned(Params.FindParam(FParams.Names[i])) then
        begin
          aPar := ParamByName(FParams.Names[i]);
          aPar.AsString:=FParams.ValueFromIndex[i];
        end;
    end;
  {
  if (FLimit>0) and Assigned(Params.FindParam('Limit')) then
    ParamByName('Limit').AsInteger:=FLimit;
  }
end;
procedure TSQLDBDataSet.SetBaseFilter(const AValue: string);
begin
  FBaseFilter := AValue;
  DoUpdateSQL;
end;
function TSQLDBDataSet.GetSQL: string;
begin
  Result := FSQL;
end;
procedure TSQLDBDataSet.SetSQL(const AValue: string);
var
  NewSQL: string;
  i: Integer;
  aPar: TParam;
begin
  if AValue=FSQL then exit;
  Close;
  Params.Clear;
  FParams.Clear;
  FSQL := AValue;
  FIntSQL := TAbstractDBModule(Owner).BuildSQL(Self);
  FFilter := '';
  FIntFilter:='';
  SQL.Text := FIntSQL;
  {
  TAbstractDBModule(Owner).DecodeFilter(AValue,FParams,NewSQL);
  Params.Clear;
  FSQL := NewSQL;
  FIntSQL := BuildSQL;
  FFilter := '';
  SQL.Text := FIntSQL;
  for i := 0 to FParams.Count-1 do
    begin
      aPar := ParamByName(FParams.Names[i]);
      aPar.AsString:=FParams.ValueFromIndex[i];
    end;
  }
  if (FLimit>0) and Assigned(Params.FindParam('Limit')) then
    ParamByName('Limit').AsInteger:=FLimit;
end;
procedure TSQLDBDataSet.Setlimit(const AValue: Integer);
begin
  if FLimit = AValue then exit;
  FLimit := AValue;
  DoUpdateSQL;
end;
procedure TSQLDBDataSet.SetSortDirection(const AValue: TSortDirection);
begin
  if (FSortDirection=AValue) and Active then exit;
  FSortDirection := AValue;
  if not GetSortLocal then
    begin
      DoUpdateSQL;
    end;
end;
procedure TSQLDBDataSet.SetSortFields(const AValue: string);
begin
  FSortFields := AValue;
end;

procedure TSQLDBDataSet.SetLocalSortFields(const AValue: string);
begin
  //SortedFields:=AValue;
end;

procedure TSQLDBDataSet.SetBaseSortFields(const AValue: string);
begin
  FBaseSortFields := AValue;
end;
procedure TSQLDBDataSet.SetSortLocal(const AValue: Boolean);
begin
  {
  if AValue then
    begin
      if FSortDirection = sdAscending then
        SortType := stAscending
      else if FSortDirection = sdDescending then
        SortType := stDescending
      else
        SortType := stIgnored;
    end
  else
    SortType := stIgnored;
  }
end;
function TSQLDBDataSet.GetFilterTables: string;
begin
  Result := FTableNames;
end;
procedure TSQLDBDataSet.SetFilterTables(const AValue: string);
begin
  if AValue = FTableNames then exit;
  FTableNames := AValue;
  DoUpdateSQL;
end;
function TSQLDBDataSet.GetUsePermissions: Boolean;
begin
  Result := FUsePermissions;
end;
procedure TSQLDBDataSet.SetUsePermisions(const AValue: Boolean);
begin
  if AValue = FUsePermissions then exit;
  FUsePermissions := AValue;
  DoUpdateSQL;
end;
function TSQLDBDataSet.GetDistinct: Boolean;
begin
  Result := FDistinct;
end;
procedure TSQLDBDataSet.SetDistinct(const AValue: Boolean);
begin
  if AValue = FDistinct then exit;
  FDistinct := AValue;
  DoUpdateSQL;
end;
function TSQLDBDataSet.GetBaseSorting: string;
begin
  Result := FBaseSorting;
end;
procedure TSQLDBDataSet.SetBaseSorting(AValue: string);
begin
  FBaseSorting := AValue;
end;

function TSQLDBDataSet.GetBaseSortDirection: TSortDirection;
begin
  Result := FBaseSortDirection;
end;
procedure TSQLDBDataSet.SetBaseSortDirection(AValue: TSortDirection);
begin
  FBaseSortDirection := AValue;
end;
function TSQLDBDataSet.GetUseBaseSorting: Boolean;
begin
  Result := FUseBaseSorting;
end;
procedure TSQLDBDataSet.SetUseBaseSorting(AValue: Boolean);
begin
  FUseBaseSorting := AValue;
  DoUpdateSQL;
end;
function TSQLDBDataSet.GetfetchRows: Integer;
begin
  result := PacketRecords;
end;
procedure TSQLDBDataSet.SetfetchRows(AValue: Integer);
begin
  if AValue = 0 then
    PacketRecords:=-1
  else
    PacketRecords:=AValue;
end;

function TSQLDBDataSet.GetParameterValue(const aName: string): Variant;
begin
  Result := ParamByName(aName).Value;
end;

procedure TSQLDBDataSet.SetParameterValue(const aName: string; AValue: Variant);
begin
  ParamByName(aName).Value := AValue;
end;

function TSQLDBDataSet.GetManagedFieldDefs: TFieldDefs;
begin
  Result := FManagedFieldDefs;
end;
function TSQLDBDataSet.GetManagedIndexDefs: TIndexDefs;
begin
  Result := FManagedIndexDefs;
end;
function TSQLDBDataSet.GetTableName: string;
begin
  Result := FDefaultTableName;
end;
procedure TSQLDBDataSet.SetTableName(const AValue: string);
begin
  FDefaultTableName := AValue;
end;

function TSQLDBDataSet.GetTableNames: string;
begin
  Result := FTableNames;
end;

function TSQLDBDataSet.GetConnection: TComponent;
begin
  Result := DataBase;
end;
function TSQLDBDataSet.GetTableCaption: string;
begin
  Result := FTableCaption;
end;
procedure TSQLDBDataSet.SetTableCaption(const AValue: string);
begin
  FTableCaption := AValue;
end;
function TSQLDBDataSet.GetUpStdFields: Boolean;
begin
  Result := FUpStdFields;
end;

procedure TSQLDBDataSet.SetUpStdFields(AValue: Boolean);
begin
  FUpStdFields := AValue;
end;

function TSQLDBDataSet.GetUpChangedBy: Boolean;
begin
  Result := FUpChangedBy;
end;

procedure TSQLDBDataSet.SetUpChangedBy(AValue: Boolean);
begin
  FUpChangedBy:=AValue;
end;

function TSQLDBDataSet.GetUseIntegrity: Boolean;
begin
  Result := FUseIntegrity;
end;
procedure TSQLDBDataSet.SetUseIntegrity(AValue: Boolean);
begin
  FUseIntegrity:=AValue;
end;

function TSQLDBDataSet.GetAsReadonly: Boolean;
begin
  result := Self.ReadOnly;
end;

procedure TSQLDBDataSet.SetAsReadonly(AValue: Boolean);
begin
  Self.ReadOnly:=AValue;
end;

procedure TSQLDBDataSet.SetConnection(aConn: TComponent);
begin
  DataBase := TSQLConnector(aConn);
end;

function TSQLDBDataSet.GetMasterdataSource: TDataSource;
begin
  Result := MasterDataSource;
end;

procedure TSQLDBDataSet.SetMasterdataSource(AValue: TDataSource);
begin
  MasterDataSource := AValue;
end;

procedure TSQLDBDataSet.SetTableNames(const AValue: string);
begin
  FTableNames:=AValue;
end;

procedure TSQLDBDataSet.SetOrigTable(AValue: TComponent);
begin
  FOrigTable := TAbstractDBDataset(AValue);
end;

function TSQLDBDataSet.GetOrigTable: TComponent;
begin
  Result := FOrigTable;
end;

procedure TSQLDBDataSet.SetDataSource(AValue: TDataSource);
begin
  DataSource := AValue;
end;

procedure TSQLDBDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  tmp: String;
begin
  inherited;
  if Assigned(FOrigTable) then
    FOrigTable.Change;
end;
function TSQLDBDataSet.GetSubDataSet(aName: string): TComponent;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FSubDataSets.Count-1 do
    with TAbstractDBDataset(FSubDataSets[i]) as IBaseManageDB do
      if TableName = aName then
        Result := TAbstractDBDataset(FSubDataSets[i]);
end;
procedure TSQLDBDataSet.RegisterSubDataSet(aDataSet: TComponent);
var
  i: Integer;
begin
  FSubDataSets.Add(aDataSet);
end;
function TSQLDBDataSet.GetCount: Integer;
begin
  try
    Result := FSubDataSets.Count;
  except
    Result := 0;
  end;
end;
function TSQLDBDataSet.GetSubDataSetIdx(aIdx: Integer): TComponent;
begin
  Result := nil;
  if aIdx < FSubDataSets.Count then
    Result := TAbstractDBDataset(FSubDataSets[aIdx]);
end;
function TSQLDBDataSet.IsChanged: Boolean;
begin
  Result := Modified;
  if Assigned(FOrigTable) then
    Result := ForigTable.Changed;
end;
constructor TSQLDBDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFirstOpen:=True;
  FSQL := '';
  fBaseSorting := '%s';
  FChangeUni:=False;
  FUseBaseSorting:=False;
  FBaseSortDirection:=sdIgnored;
  FManagedFieldDefs := TFieldDefs.Create(Self);
  FManagedIndexDefs := TIndexDefs.Create(Self);
  FSubDataSets := TList.Create;
  FUsePermissions := False;
  FOrigTable := nil;
  //SortType := stIgnored;
  FUpStdFields := True;
  FUpChangedBy := True;
  FUseIntegrity:=False;//disable for sync
  FParams := TStringList.Create;
  FInBeforePost := False;
end;
destructor TSQLDBDataSet.Destroy;
var
  aItem: TObject;
begin
  FParams.Free;
  FManagedFieldDefs.Free;
  FManagedIndexDefs.Free;
  FSubDataSets.Free;
  try
    inherited Destroy;
  except
  end;
end;

procedure TSQLDBDataSet.DoExecSQL;
begin
  ExecSQL;
end;

function TSQLDBDataSet.NumRowsAffected: Integer;
begin
  Result := RowsAffected;
end;

procedure TSQLDBDataSet.Post;
begin
  inherited Post;
  ApplyUpdates;
end;

initialization
  ConnectionClass:=TSQLConnection;
  QueryClass:=TSQLDBDataSet;
end.


