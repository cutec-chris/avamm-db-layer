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
Created 01.06.2006
*******************************************************************************}
unit uZeosDBDM;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, ZConnection, ZSqlMetadata,
  ZAbstractRODataset, ZDataset, ZSequence,ZAbstractConnection,
  ZSqlMonitor,uBaseDatasetInterfaces,syncobjs,
  ZCompatibility,dateutils,
  uAbstractDBLayer;
type
  TUnprotectedDataSet = class(TDataSet);
  { TZeosConnection }

  TZeosConnection = class(TZConnection,IBaseDBConnection)
    procedure MonitorLogTrace(Sender: TObject; Event: TZLoggingEvent);
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
  protected
  end;

  { TZeosDBDataSet }

  TZeosDBDataSet = class(TZQuery,IBaseDBFilter,IBaseManageDB,IBaseSubDatasets,IBaseModifiedDS)
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
    function IndexExists(IndexName : string) : Boolean;
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
  end;
implementation
uses ZDbcIntfs,uEncrypt,ZDbcSqLite;
resourcestring
  strUnknownDbType                = 'Unbekannter Datenbanktyp';
  strDatabaseConnectionLost       = 'Die Datenbankverbindung wurde verloren !';

var
  Monitor : TZSQLMonitor;

{ TZeosConnection }

function TZeosConnection.DoSetProperties(aProp: string): Boolean;
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
      Disconnect;
    Port:=0;
    Properties.Clear;
    Properties.Add('timeout=3');
    Protocol:='';
    User:='';
    Password:='';
    HostName:='';
    Database:='';
    Properties.Clear;
    if copy(tmp,0,pos(';',tmp)-1) <> 'sqlite-3-edata' then
      Protocol:=copy(tmp,0,pos(';',tmp)-1)
    else
      begin
        Protocol:='sqlite-3';
        FEData:=True;
      end;
    Assert(Protocol<>'',strUnknownDbType);
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
    Database:=copy(tmp,0,pos(';',tmp)-1);
    FDatabaseDir:=ExtractFileDir(ExpandFileName(Database));
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    User := copy(tmp,0,pos(';',tmp)-1);
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    if copy(tmp,0,1) = 'x' then
      Password := Decrypt(copy(tmp,2,length(tmp)),99998)
    else
      Password := tmp;
    if (copy(Protocol,0,6) = 'sqlite')
    then
      begin
//        if Connection=MainConnection then //Dont check this on attatched db´s (we want to create them on the fly)
//          if not FileExists(Database) then
//            raise Exception.Create('Databasefile dosend exists');
        TransactIsolationLevel:=tiNone;
      end;
    if (copy(Protocol,0,8) = 'postgres')
    then
      begin
        Properties.Add('compression=true');
        {$IFDEF CPUARM}
        Properties.Add('sslmode=disable');
        {$ENDIF}
        TransactIsolationLevel:=tiNone;
        AutoEncodeStrings:=true;
      end
    else if (copy(Protocol,0,8) = 'firebird')
    or (copy(Protocol,0,9) = 'interbase')
    then
      begin
        TransactIsolationLevel:=tiReadCommitted;
      end
    else if (copy(Protocol,0,5) = 'mysql') then
      begin
        TransactIsolationLevel:=tiReadUncommitted;
        Properties.Clear;
        Properties.Add('compression=true');
        //Properties.Add('codepage=UTF8');
        //Properties.Add('timeout=0');
        Properties.Add('ValidateUpdateCount=-1');
        Properties.Add('MYSQL_OPT_RECONNECT=TRUE');
      end
    else if (copy(Protocol,0,5) = 'mssql') then
      begin
        TransactIsolationLevel:=tiReadUncommitted;
        AutoEncodeStrings:=true;
      end;
    Properties.Add('Undefined_Varchar_AsString_Length= 255');
  finally
  end;
end;
function TZeosConnection.DoCreateDBFromProperties(aProp: string): Boolean;
var
  aDatabase: String;
  aUser: String;
  aPassword: String;
begin
  DoSetProperties(aProp);
  aDatabase := Database;
  aUser := User;
  aPassword := Password;
  if (copy(Protocol,0,8) = 'postgres')
  then
    begin
      Database:='postgres';
    end
    else if (copy(Protocol,0,5) = 'mssql') then
      Properties.Add('CreateNewDatabase=CREATE DATABASE "'+aDatabase+'"')
    else if (copy(Protocol,0,8) = 'firebird')
    or (copy(Protocol,0,9) = 'interbase')
    then
      begin
        if HostName <> '' then
          Properties.Add('CreateNewDatabase=CREATE DATABASE '''+HostName+':'+aDatabase+''' USER '''+aUser+''' PASSWORD '''+aPassword+''' PAGE_SIZE = 4096 DEFAULT CHARACTER SET UTF8')
        else
          Properties.Add('CreateNewDatabase=CREATE DATABASE '''+aDatabase+''' USER '''+aUser+''' PASSWORD '''+aPassword+''' PAGE_SIZE = 4096 DEFAULT CHARACTER SET UTF8');
      end
    else if (copy(Protocol,0,6) = 'sqlite') then
      begin
        if ExtractFileDir(Database)<>'' then
          ForceDirectories(ExtractFileDir(Database));
      end;
    try
      Connected:=True;
    except
      on e : Exception do
//      if Assigned(BaseApplication) then
//        with BaseApplication as IBaseDBInterface do
//          LastError := e.Message;
    end;
    if (copy(Protocol,0,8) = 'postgres')
    then
      begin
        Result := ExecuteDirect('CREATE DATABASE "'+aDatabase+'" WITH OWNER = "'+aUser+'" ENCODING = ''UTF8'' CONNECTION LIMIT = -1;');
        Disconnect;
        Database:=aDatabase;
      end;
    Connected:=True;
    Result := Connected;
    Disconnect;
end;
function TZeosConnection.DoInitializeConnection: Boolean;
begin
  if not Assigned(Monitor) then
    begin
      Monitor := TZSQLMonitor.Create(Owner);
      Monitor.OnLogTrace:=@MonitorLogTrace;
      Monitor.Active:=True;
    end;
  Result := True;
  FLimitAfterSelect := False;
  FDBTyp:=Protocol;
  FLimitSTMT := 'LIMIT %s';
  if Protocol = 'sqlite-3' then
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
  else if (copy(Protocol,0,8) = 'firebird')
       or (copy(Protocol,0,9) = 'interbase') then
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
  else if Protocol = 'mssql' then
    begin
      FLimitAfterSelect := True;
      FLimitSTMT := 'TOP %s';
    end
  else if (copy(Protocol,0,8) = 'postgres') then
    begin
      FDBTyp := 'postgres';

    end;

end;

procedure TZeosConnection.MonitorLogTrace(Sender: TObject; Event: TZLoggingEvent
  );
begin
  try
    if pos('--debug-sql',lowercase(cmdline))>0 then
      if Assigned(Owner) and Assigned(TAbstractDBModule(Owner).OnLog) then
        TAbstractDBModule(Owner).OnLog(Owner,Event.AsString);
  except
  end;
end;

function TZeosConnection.DoExecuteDirect(aSQL: string): Integer;
begin
  ExecuteDirect(aSQL);
end;
function TZeosConnection.DoStartTransaction(ForceTransaction: Boolean): Boolean;
begin
  Tag := Integer(TransactIsolationLevel);
  if ForceTransaction and (copy(Protocol,0,6) = 'sqlite') then
    TransactIsolationLevel:=tiReadCommitted
  else if (copy(Protocol,0,8) = 'postgres') then
    TransactIsolationLevel:=tiReadCommitted
  else if (copy(Protocol,0,5) = 'mssql') then
    TransactIsolationLevel:=tiReadUnCommitted;
  StartTransaction;
  FInTransaction:=True;
end;
function TZeosConnection.DoCommitTransaction: Boolean;
begin
  if not AutoCommit then
    Commit;
  if TZTransactIsolationLevel(Tag) <> TransactIsolationLevel then
    TransactIsolationLevel := TZTransactIsolationLevel(Tag);
  FInTransaction:=False;
end;
function TZeosConnection.DoRollbackTransaction: Boolean;
begin
  if not AutoCommit then
    Rollback;
  if TZTransactIsolationLevel(Tag) <> TransactIsolationLevel then
    TransactIsolationLevel := TZTransactIsolationLevel(Tag);
  FInTransaction:=False;
end;

function TZeosConnection.IsTransactionActive: Boolean;
begin
  result := FInTransaction;
end;

procedure TZeosConnection.DoDisconnect;
begin
  try
    Disconnect;
  except
  end;
end;
procedure TZeosConnection.DoConnect;
begin
  FInTransaction:=False;
  try
    Connect;
  except on e : Exception do
    begin
//      if Assigned(BaseApplication) then
//        with BaseApplication as IBaseDBInterface do
//          LastError := e.Message;
    end;
  end;
end;

function TZeosConnection.GetHandle: Pointer;
begin
  Result := (DbcConnection as IZSQLiteConnection).GetConnectionHandle;
end;

function TZeosConnection.GetDatabaseName: string;
begin
  Result := Database;
end;

function TZeosConnection.GetUniID(aConnection: TComponent; Generator: string;
  Tablename: string; AutoInc: Boolean): Variant;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  bConnection: TComponent;
  aId: Int64 = 0;
begin
  with TAbstractDBModule(Owner) do
   begin
      Result := 0;
      try
        while aId=Result do
          begin
            if (copy(Protocol,0,6) = 'sqlite') then
              Statement := DbcConnection.CreateStatement //we have global locking in sqlite so we must use the actual connection
            else
              Statement := TZConnection(MainConnection).DbcConnection.CreateStatement;
            if AutoInc then
              begin
                if (copy(Protocol,0,5) = 'mysql') then
                  begin
                    Statement.Execute('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'='+QuoteField('ID')+'+1;')
                  end
                else
                  begin
                    if LimitAfterSelect then
                      Statement.Execute('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'=(select '+Format(LimitSTMT,['1'])+' '+QuoteField('ID')+' from '+QuoteField(Generator)+')+1;')
                    else
                      Statement.Execute('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'=(select '+QuoteField('ID')+' from '+QuoteField(Generator)+' '+Format(LimitSTMT,['1'])+')+1;');
                  end;
              end;
            try
              ResultSet := Statement.ExecuteQuery('SELECT '+QuoteField('ID')+' FROM '+QuoteField(Generator));
              if ResultSet.Next then
                Result := ResultSet.GetLong(1)
              else
                begin
                  Statement.Execute('insert into '+QuoteField(GENERATOR)+' ('+QuoteField('SQL_ID')+','+QuoteField('ID')+') VALUES (1,1000);');
                  Result := 1000;
                  ResultSet.Close;
                  Statement.Close;
                  break;
                end;
              try
                if Tablename<>'' then
                  begin
                    ResultSet := Statement.ExecuteQuery('SELECT '+QuoteField('SQL_ID')+' FROM '+QuoteField(Tablename)+' WHERE '+QuoteField('SQL_ID')+'='+QuoteValue(Format('%d',[Int64(Result)])));
                    if ResultSet.Next then
                      aId := ResultSet.GetLong(1)
                  end;
              except
              end;
              ResultSet.Close;
              Statement.Close;
            except
            end;
          end;
        except
        end;
    end;
end;

function TZeosConnection.DoGetTableNames(aTables: TStrings): Boolean;
begin
  Result := True;
  GetTableNames('','',aTables);
end;
function TZeosConnection.DoGetTriggerNames(aTriggers: TStrings): Boolean;
begin
  Result := True;
  Self.GetTriggerNames('','',aTriggers);
end;

function TZeosConnection.DoGetColumns(aTableName: string): TStrings;
var
  Metadata: IZDatabaseMetadata;
begin
  Metadata := DbcConnection.GetMetadata;
  Result := TStringList.Create;
  with Metadata.GetColumns(Catalog,'',aTableName,'') do
   try
     while Next do
       Result.Add(GetStringByName('COLUMN_NAME'));
   finally
     Close;
   end;
end;

function TZeosConnection.DoGetIndexes(aTableName: string): TStrings;
var
  Metadata: IZDatabaseMetadata;
begin
  Metadata := DbcConnection.GetMetadata;
  Result := TStringList.Create;
  with Metadata.GetIndexInfo(Catalog,'',aTableName,False,False) do
   try
     while Next do
       Result.Add(GetStringByName('COLUMN_NAME'));
   finally
     Close;
   end;
end;

function TZeosConnection.IsConnected: Boolean;
begin
  Result := Connected;
end;
function TZeosConnection.GetLimitAfterSelect: Boolean;
begin
  Result := FLimitAfterSelect;
end;
function TZeosConnection.GetLimitSTMT: string;
begin
  Result := FLimitSTMT;
end;

function TZeosConnection.DoGetDBLayerType: string;
begin
  if FDBTyp = '' then
    Result := Protocol
  else Result := FDBTyp;
end;

function TZeosConnection.GetSyncOffset: Integer;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  bConnection: TComponent;
begin
  {
  if Assigned(Sequence) then
    begin
      bConnection := MainConnection;
      Sequence.Connection := TZConnection(bConnection);
      Result := Sequence.GetCurrentValue shr 56;
      Sequence.Connection := nil;
    end
  else}
    begin
      Statement := TZConnection(TAbstractDBModule(Owner).MainConnection).DbcConnection.CreateStatement;
      ResultSet := Statement.ExecuteQuery('SELECT '+TAbstractDBModule(Owner).QuoteField('ID')+' FROM '+TAbstractDBModule(Owner).QuoteField('GEN_SQL_ID'));
      if ResultSet.Next then
        Result := ResultSet.GetLong(1) shr 56
      else Result := 0;
      ResultSet.Close;
      Statement.Close;
    end;
end;

procedure TZeosConnection.SetSyncOffset(const AValue: Integer);
var
  Statement: IZStatement;
  aVal: Int64;
begin
  aVal := AValue;
  aVal := aVal shl 56;
  {if Assigned(Sequence) then
    begin
      raise Exception.Create('Not implemented !!!');
    end
  else}
    begin
      Statement := TZConnection(TAbstractDBModule(Owner).MainConnection).DbcConnection.CreateStatement;
      Statement.Execute('update '+TAbstractDBModule(Owner).QuoteField('GEN_SQL_ID')+' set '+TAbstractDBModule(Owner).QuoteField('ID')+'='+IntToStr(aVal));
      Statement.Close;
    end;
end;

procedure TZeosDBDataSet.TDateTimeFieldGetText(Sender: TField;
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
procedure TZeosDBDataSet.SetNewIDIfNull;
begin
  if (FieldDefs.IndexOf('AUTO_ID') = -1) and (FieldDefs.IndexOf('SQL_ID') > -1) and  FieldByName('SQL_ID').IsNull then
    begin
      with Self as IBaseManageDB do
        FieldByName('SQL_ID').AsVariant:=TAbstractDBModule(Self.Owner).GetUniID(Connection,'GEN_SQL_ID',TableName);
      FHasNewID:=True;
    end
  else if (FieldDefs.IndexOf('SQL_ID') = -1) and (FieldDefs.IndexOf('AUTO_ID') > -1) and FieldByName('AUTO_ID').IsNull then
    begin
      with Self as IBaseManageDB do
        FieldByName('AUTO_ID').AsVariant:=TAbstractDBModule(Self.Owner).GetUniID(Connection,'GEN_AUTO_ID',TableName);
      FHasNewID:=True;
    end;
end;

function TZeosDBDataSet.IndexExists(IndexName: string): Boolean;
var
  Metadata: TZSQLMetadata;
  CustomQuery: TZQuery;
begin
  CustomQuery := TZQuery.Create(Self);
  CustomQuery.Connection := Connection;
  if (copy(TZConnection(TAbstractDBModule(Owner).MainConnection).Protocol,0,8) = 'firebird')
  or (copy(TZConnection(TAbstractDBModule(Owner).MainConnection).Protocol,0,9) = 'interbase') then
    begin
      CustomQuery.SQL.Text := 'select rdb$index_name from rdb$indices where rdb$index_name='+TAbstractDBModule(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TZConnection(TAbstractDBModule(Owner).MainConnection).Protocol,0,6) = 'sqlite') then
    begin
      CustomQuery.SQL.Text := 'select name from SQLITE_MASTER where "TYPE"=''index'' and NAME='+TAbstractDBModule(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TZConnection(TAbstractDBModule(Owner).MainConnection).Protocol,0,5) = 'mssql') then
    begin
      CustomQuery.SQL.Text := 'select name from dbo.sysindexes where NAME='+TAbstractDBModule(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TZConnection(TAbstractDBModule(Owner).MainConnection).Protocol,0,8) = 'postgres') then
    begin
      CustomQuery.SQL.Text := 'select * from pg_class where relname='+TAbstractDBModule(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else
    begin
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
    end;
  CustomQuery.Free;
end;

procedure TZeosDBDataSet.WaitForLostConnection;
var
  aConnThere: Boolean;
begin
  if not TAbstractDBModule(Owner).Ping(Connection) then
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
            if TAbstractDBModule(Owner).Ping(Connection) then
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

procedure TZeosDBDataSet.DoUpdateSQL;
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

procedure TZeosDBDataSet.InternalOpen;
var
  a: Integer;
begin
  if (not Assigned(Connection)) or (not Connection.Connected) then exit;
  if Connection.Protocol='mysql' then
    Properties.Values['ValidateUpdateCount'] := 'False';
  {$if FPC_FULLVERSION>30000}
  if Assigned(FOrigTable) and Assigned(ForigTable.DataModule) then
    TAbstractDBModule(ForigTable.DataModule).LastTime := GetTickCount64;
  {$endif}
  if TAbstractDBModule(Owner).IgnoreOpenRequests then exit;
  if FFirstOpen then
    begin
      FIntSQL := TAbstractDBModule(Owner).BuildSQL(Self);
      SQL.Text := FIntSQL;
      if (FLimit>0) and Assigned(Params.FindParam('Limit')) and ((copy(Connection.Protocol,0,8)<>'postgres') or (FLimit>90))  then
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

procedure TZeosDBDataSet.InternalRefresh;
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
        if TAbstractDBModule(Owner).Ping(Connection) then
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

procedure TZeosDBDataSet.InternalPost;
begin
  inherited InternalPost;
end;

procedure TZeosDBDataSet.DoAfterInsert;
begin
  inherited DoAfterInsert;
  if Assigned(FOrigTable) then
    begin
      FOrigTable.DisableChanges;
      FOrigTable.FillDefaults(Self);
      FOrigTable.EnableChanges;
    end;
end;
procedure TZeosDBDataSet.DoBeforePost;
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
procedure TZeosDBDataSet.DoBeforeInsert;
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

procedure TZeosDBDataSet.DoBeforeEdit;
begin
  inherited DoBeforeEdit;
end;

procedure TZeosDBDataSet.DoBeforeDelete;
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
procedure TZeosDBDataSet.DoAfterDelete;
begin
  inherited DoAfterDelete;
  if Assigned(FOrigTable) then
    FOrigTable.Change;
end;
procedure TZeosDBDataSet.DoAfterScroll;
begin
  inherited DoAfterScroll;
  if Assigned(ForigTable) then
    FOrigTable.UnChange;
end;

procedure TZeosDBDataSet.DoBeforeCancel;
begin
  inherited DoBeforeCancel;
  if State = dsInsert then
    begin
      if Assigned(FOrigTable) and Assigned(FOrigTable.OnRemove) then FOrigTable.OnRemove(FOrigTable);
    end;
end;

function TZeosDBDataSet.GetFields: string;
begin
  Result := FFields;
end;
function TZeosDBDataSet.GetFilter: string;
begin
  Result := FFilter;
end;

function TZeosDBDataSet.GetIntFilter: string;
begin
  Result := FIntFilter;
end;

function TZeosDBDataSet.GetBaseFilter: string;
begin
  Result := FBaseFilter;
end;
function TZeosDBDataSet.GetLimit: Integer;
begin
  Result := FLimit;
end;
function TZeosDBDataSet.GetSortDirection: TSortDirection;
begin
  Result := FSortDirection;
end;
function TZeosDBDataSet.GetSortFields: string;
begin
  Result := FSortFields;
end;

function TZeosDBDataSet.GetLocalSortFields: string;
begin
  Result := SortedFields;
end;

function TZeosDBDataSet.GetBaseSortFields: string;
begin
  Result := FBaseSortFields;
end;
function TZeosDBDataSet.GetSortLocal: Boolean;
begin
  Result := SortType <> stIgnored;
end;
procedure TZeosDBDataSet.SetFields(const AValue: string);
begin
  if FFields<>AValue then
    begin
      FFields := AValue;
      DoUpdateSQL;
    end;
end;
procedure TZeosDBDataSet.SetFilter(const AValue: string);
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
  if (FLimit>0) and Assigned(Params.FindParam('Limit')) then
    ParamByName('Limit').AsInteger:=FLimit;
end;
procedure TZeosDBDataSet.SetBaseFilter(const AValue: string);
begin
  FBaseFilter := AValue;
  DoUpdateSQL;
end;
function TZeosDBDataSet.GetSQL: string;
begin
  Result := FSQL;
end;
procedure TZeosDBDataSet.SetSQL(const AValue: string);
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
procedure TZeosDBDataSet.Setlimit(const AValue: Integer);
begin
  if FLimit = AValue then exit;
  FLimit := AValue;
  DoUpdateSQL;
end;
procedure TZeosDBDataSet.SetSortDirection(const AValue: TSortDirection);
begin
  if (FSortDirection=AValue) and Active then exit;
  FSortDirection := AValue;
  if not GetSortLocal then
    begin
      DoUpdateSQL;
    end;
end;
procedure TZeosDBDataSet.SetSortFields(const AValue: string);
begin
  FSortFields := AValue;
end;

procedure TZeosDBDataSet.SetLocalSortFields(const AValue: string);
begin
  SortedFields:=AValue;
end;

procedure TZeosDBDataSet.SetBaseSortFields(const AValue: string);
begin
  FBaseSortFields := AValue;
end;
procedure TZeosDBDataSet.SetSortLocal(const AValue: Boolean);
begin
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
end;
function TZeosDBDataSet.GetFilterTables: string;
begin
  Result := FTableNames;
end;
procedure TZeosDBDataSet.SetFilterTables(const AValue: string);
begin
  if AValue = FTableNames then exit;
  FTableNames := AValue;
  DoUpdateSQL;
end;
function TZeosDBDataSet.GetUsePermissions: Boolean;
begin
  Result := FUsePermissions;
end;
procedure TZeosDBDataSet.SetUsePermisions(const AValue: Boolean);
begin
  if AValue = FUsePermissions then exit;
  FUsePermissions := AValue;
  DoUpdateSQL;
end;
function TZeosDBDataSet.GetDistinct: Boolean;
begin
  Result := FDistinct;
end;
procedure TZeosDBDataSet.SetDistinct(const AValue: Boolean);
begin
  if AValue = FDistinct then exit;
  FDistinct := AValue;
  DoUpdateSQL;
end;
function TZeosDBDataSet.GetBaseSorting: string;
begin
  Result := FBaseSorting;
end;
procedure TZeosDBDataSet.SetBaseSorting(AValue: string);
begin
  FBaseSorting := AValue;
end;

function TZeosDBDataSet.GetBaseSortDirection: TSortDirection;
begin
  Result := FBaseSortDirection;
end;
procedure TZeosDBDataSet.SetBaseSortDirection(AValue: TSortDirection);
begin
  FBaseSortDirection := AValue;
end;
function TZeosDBDataSet.GetUseBaseSorting: Boolean;
begin
  Result := FUseBaseSorting;
end;
procedure TZeosDBDataSet.SetUseBaseSorting(AValue: Boolean);
begin
  FUseBaseSorting := AValue;
  DoUpdateSQL;
end;
function TZeosDBDataSet.GetfetchRows: Integer;
begin
  result := FetchRow;
end;
procedure TZeosDBDataSet.SetfetchRows(AValue: Integer);
begin
  FetchRow:=AValue;
end;

function TZeosDBDataSet.GetParameterValue(const aName: string): Variant;
begin
  Result := ParamByName(aName).Value;
end;

procedure TZeosDBDataSet.SetParameterValue(const aName: string; AValue: Variant);
begin
  ParamByName(aName).Value := AValue;
end;

function TZeosDBDataSet.GetManagedFieldDefs: TFieldDefs;
begin
  Result := FManagedFieldDefs;
end;
function TZeosDBDataSet.GetManagedIndexDefs: TIndexDefs;
begin
  Result := FManagedIndexDefs;
end;
function TZeosDBDataSet.GetTableName: string;
begin
  Result := FDefaultTableName;
end;
procedure TZeosDBDataSet.SetTableName(const AValue: string);
begin
  FDefaultTableName := AValue;
end;

function TZeosDBDataSet.GetTableNames: string;
begin
  Result := FTableNames;
end;

function TZeosDBDataSet.GetConnection: TComponent;
begin
  Result := Connection;
end;
function TZeosDBDataSet.GetTableCaption: string;
begin
  Result := FTableCaption;
end;
procedure TZeosDBDataSet.SetTableCaption(const AValue: string);
begin
  FTableCaption := AValue;
end;
function TZeosDBDataSet.GetUpStdFields: Boolean;
begin
  Result := FUpStdFields;
end;

procedure TZeosDBDataSet.SetUpStdFields(AValue: Boolean);
begin
  FUpStdFields := AValue;
end;

function TZeosDBDataSet.GetUpChangedBy: Boolean;
begin
  Result := FUpChangedBy;
end;

procedure TZeosDBDataSet.SetUpChangedBy(AValue: Boolean);
begin
  FUpChangedBy:=AValue;
end;

function TZeosDBDataSet.GetUseIntegrity: Boolean;
begin
  Result := FUseIntegrity;
end;
procedure TZeosDBDataSet.SetUseIntegrity(AValue: Boolean);
begin
  FUseIntegrity:=AValue;
end;

function TZeosDBDataSet.GetAsReadonly: Boolean;
begin
  result := Self.ReadOnly;
end;

procedure TZeosDBDataSet.SetAsReadonly(AValue: Boolean);
begin
  Self.ReadOnly:=AValue;
end;

procedure TZeosDBDataSet.SetConnection(aConn: TComponent);
begin
  Connection := TZConnection(aConn);
end;

function TZeosDBDataSet.GetMasterdataSource: TDataSource;
begin
  Result := MasterDataSource;
end;

procedure TZeosDBDataSet.SetMasterdataSource(AValue: TDataSource);
begin
  MasterDataSource := AValue;
end;

procedure TZeosDBDataSet.SetTableNames(const AValue: string);
begin
  FTableNames:=AValue;
end;

procedure TZeosDBDataSet.SetOrigTable(AValue: TComponent);
begin
  FOrigTable := TAbstractDBDataset(AValue);
end;

function TZeosDBDataSet.GetOrigTable: TComponent;
begin
  Result := FOrigTable;
end;

procedure TZeosDBDataSet.SetDataSource(AValue: TDataSource);
begin
  DataSource := AValue;
end;

procedure TZeosDBDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  tmp: String;
begin
  inherited;
  if Assigned(FOrigTable) then
    FOrigTable.Change;
end;
function TZeosDBDataSet.GetSubDataSet(aName: string): TComponent;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FSubDataSets.Count-1 do
    with TAbstractDBDataset(FSubDataSets[i]) as IBaseManageDB do
      if TableName = aName then
        Result := TAbstractDBDataset(FSubDataSets[i]);
end;
procedure TZeosDBDataSet.RegisterSubDataSet(aDataSet: TComponent);
var
  i: Integer;
begin
  FSubDataSets.Add(aDataSet);
end;
function TZeosDBDataSet.GetCount: Integer;
begin
  Result := FSubDataSets.Count;
end;
function TZeosDBDataSet.GetSubDataSetIdx(aIdx: Integer): TComponent;
begin
  Result := nil;
  if aIdx < FSubDataSets.Count then
    Result := TAbstractDBDataset(FSubDataSets[aIdx]);
end;
function TZeosDBDataSet.IsChanged: Boolean;
begin
  Result := Modified;
  if Assigned(FOrigTable) then
    Result := ForigTable.Changed;
end;
constructor TZeosDBDataSet.Create(AOwner: TComponent);
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
  SortType := stIgnored;
  FUpStdFields := True;
  FUpChangedBy := True;
  FUseIntegrity:=False;//disable for sync
  FParams := TStringList.Create;
  FInBeforePost := False;
end;
destructor TZeosDBDataSet.Destroy;
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

procedure TZeosDBDataSet.DoExecSQL;
begin
  ExecSQL;
end;

function TZeosDBDataSet.NumRowsAffected: Integer;
begin
  Result := RowsAffected;
end;

initialization
  ConnectionClass:=TZeosConnection;
  QueryClass:=TZeosDBDataSet;
  Monitor := nil;
end.


