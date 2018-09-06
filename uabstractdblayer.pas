unit uAbstractDBLayer;

interface

uses
  Classes,db,uBaseDatasetInterfaces,Sysutils,syncobjs;

type

  { TInternalDBDataSet }

  TInternalDBDataSet = class
  private
    FDataSet: TDataSet;
  public
    destructor Destroy;override;
    property DataSet : TDataSet read FDataSet write FDataSet;
  end;

  { TAbstractDBConnection }

  TAbstractDBConnection = class(TComponent)
  public
    function DoSetProperties(aProp : string) : Boolean;
    function DoCreateDBFromProperties(aProp : string) : Boolean;
    function Handle : Pointer;
  end;

  { TAbstractDBQuery }

  TAbstractDBQuery = class(TDataSet)
  private
    function GetFetchRows: Integer;
    function GetSQL: string;
    procedure SetFetchRows(AValue: Integer);
    procedure SetSQL(AValue: string);
  public
    property SQL : string read GetSQL write SetSQL;
    property FetchRows : Integer read GetFetchRows write SetFetchRows;
  end;//IBaseDBFilter,IBaseManageDB,IBaseSubDatasets,IBaseModifiedDS

  TAbstractDBLogEvent = procedure(Sender : TComponent;Log : string) of Object;

  { TAbstractDBModule }

  TAbstractDBModule = class(TComponent)
  private
    FCheckedTables : TStrings;
    FConnect: TNotifyEvent;
    FConnectionLost: TNotifyEvent;
    FCS: TCriticalSection;
    FIgnoreOpenrequests: Boolean;
    FKeepAlive: TNotifyEvent;
    FLastStmt: string;
    FLastTime: Int64;
    FLog: TAbstractDBLogEvent;
    FTables: TStrings;
    FTriggers: TStrings;
    FProperties: String;
    FDatabaseDir : string;
    FUseParameters: Boolean;
    FUsersFilter: string;
    FUseExtData : Boolean;
    FMainConnection: TAbstractDBConnection;
    function GetSyncOffset: Integer;virtual;
    procedure SetSyncOffset(AValue: Integer);virtual;
  protected
    function GetConnection: TAbstractDBConnection;virtual;
    function GetLimitAfterSelect: Boolean;virtual;
    function GetLimitSTMT: string;virtual;
    function GetDataSetClass : TDatasetClass;virtual;
    function GetConnectionClass : TComponentClass;virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MainConnection : TAbstractDBConnection read GetConnection;

    function GetNewDataSet(aTable : TAbstractDBDataset;aConnection : TComponent = nil;MasterData : TDataSet = nil;aTables : string = '') : TDataSet;virtual;
    function GetNewDataSet(aSQL : string;aConnection : TComponent = nil;MasterData : TDataSet = nil;aOrigtable : TAbstractDBDataset = nil) : TDataSet;virtual;
    function GetNewConnection(Connected : Boolean = True) : TComponent;virtual;
    function StartTransaction(aConnection : TComponent;ForceTransaction : Boolean = False): Boolean;virtual;
    function CommitTransaction(aConnection : TComponent): Boolean;virtual;
    function RollbackTransaction(aConnection : TComponent): Boolean;virtual;
    function IsTransactionActive(aConnection : TComponent): Boolean;virtual;
    function Ping(aConnection : TComponent): Boolean;virtual;

    property CheckedTables : TStrings read FCheckedTables;
    function ShouldCheckTable(aTableName : string;SetChecked : Boolean = False) : Boolean;
    function RemoveCheckTable(aTableName : string) : Boolean;
    property Tables : TStrings read FTables;
    property Triggers : TStrings read FTriggers;
    function DBExists : Boolean;
    function GetFullTableName(aTable: string;DoLookup : Boolean = True): string;virtual;
    property LimitAfterSelect : Boolean read GetLimitAfterSelect;
    property LimitSTMT : string read GetLimitSTMT;
    function IsSQLDB : Boolean;virtual;
    property SyncOffset : Integer read GetSyncOffset write SetSyncOffset;

    function SetProperties(aProp : string;Connection : TAbstractDBConnection = nil) : Boolean;virtual;
    function CreateDBFromProperties(aProp : string;Connection : TAbstractDBConnection = nil) : Boolean;virtual;
    function TableExists(aTableName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;virtual;
    function TriggerExists(aTriggerName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;virtual;

    function ExecuteDirect(aSQL: string; aConnection: TComponent=nil): Integer;virtual;

    function BuildSQL(DataSet: TDataSet): string;
    function GetUniID(aConnection : TComponent = nil;Generator : string = 'GEN_SQL_ID';Tablename : string = '';AutoInc : Boolean = True) : Variant;virtual;
    function DateToFilter(aValue : TDateTime) : string;virtual;
    function DateTimeToFilter(aValue : TDateTime) : string;virtual;
    function QuoteField(aField : string) : string;virtual;
    function QuoteValue(aValue : string) : string;virtual;
    function EscapeString(aValue : string) : string;virtual;
    function GetDBType : string;virtual;
    function GetDBLayerType : string;virtual;
    function FieldToSQL(aName : string;aType : TFieldType;aSize : Integer;aRequired : Boolean) : string;
    function GetColumns(aTableName : string) : TStrings;
    function DecodeFilter(aSQL : string;Parameters : TStringList;var NewSQL : string) : Boolean;virtual;
    function CheckForInjection(aFilter : string) : Boolean;
    function GetUSerCode : string;virtual;
    procedure DestroyDataSet(DataSet : TDataSet);virtual;

    function BlobFieldToFile(DataSet : TDataSet;Fieldname : string;Filename : string;aSize : Integer = -1) : Boolean;virtual;
    procedure FileToBlobField(Filename : string;DataSet : TDataSet;Fieldname : string);virtual;
    procedure StreamToBlobField(Stream : TStream;aDataSet : TDataSet;Fieldname : string;Tablename : string = '');virtual;
    function BlobFieldToStream(aDataSet: TDataSet; Fieldname: string;
      dStream: TStream;aSize : Integer = -1) : Boolean; virtual;
    function BlobFieldStream(DataSet: TDataSet; Fieldname: string;Tablename : string = '') : TStream; virtual;

    property LastStatement : string read FLastStmt write FLastStmt;
    property LastTime : Int64 read FLastTime write FLastTime;
    property ParameteriseSQL : Boolean read FUseParameters write FUseParameters;
    property CriticalSection : TCriticalSection read FCS;
    property IgnoreOpenRequests : Boolean read FIgnoreOpenrequests write FIgnoreOpenrequests;
    property UsersFilter : string read FUsersFilter write FUsersFilter;
    property OnLog : TAbstractDBLogEvent read FLog write FLog;
    property OnConnectionLost : TNotifyEvent read FConnectionLost write FConnectionLost;
    property OnConnect : TNotifyEvent read FConnect write FConnect;
    property OnDisconnectKeepAlive : TNotifyEvent read FKeepAlive write FKeepAlive;
  end;

var
  QueryClass : TDatasetClass;
  ConnectionClass : TComponentClass;
resourcestring
  strSQLInjection                = 'Versuchte SQL Injection !';
implementation

function UniToSys(s : string) : string;
begin
  Result := s;
end;

{ TAbstractDBQuery }

function TAbstractDBQuery.GetFetchRows: Integer;
begin
  Result := (Self as IBaseDbFilter).GetfetchRows;
end;

function TAbstractDBQuery.GetSQL: string;
begin
  Result := (Self as IBaseDbFilter).GetSQL;
end;

procedure TAbstractDBQuery.SetFetchRows(AValue: Integer);
begin
  (Self as IBaseDbFilter).SetfetchRows(AValue);
end;

procedure TAbstractDBQuery.SetSQL(AValue: string);
begin
  (Self as IBaseDbFilter).FullSQL := AValue;
end;

{ TAbstractDBConnection }

function TAbstractDBConnection.DoSetProperties(aProp: string): Boolean;
begin
  Result := (Self as IBaseDBConnection).DoSetProperties(aProp);
end;

function TAbstractDBConnection.DoCreateDBFromProperties(aProp: string): Boolean;
begin
  Result := (Self as IBaseDBConnection).DoCreateDBFromProperties(aProp);
end;

function TAbstractDBConnection.Handle: Pointer;
begin
  Result := (Self as IBaseDBConnection).GetHandle;
end;

{ TInternalDBDataSet }

destructor TInternalDBDataSet.Destroy;
begin
  FreeAndNil(FDataSet);
end;

{ TAbstractDBModule }

function TAbstractDBModule.DBExists: Boolean;
begin
  Result := TableExists('USERS') and TableExists('GEN_SQL_ID') and TableExists('GEN_AUTO_ID');
end;

function TAbstractDBModule.GetFullTableName(aTable: string; DoLookup: Boolean
  ): string;
begin
  Result := aTable;
end;

function TAbstractDBModule.IsSQLDB: Boolean;
begin
  Result := GetDBLayerType='SQL';
end;

function TAbstractDBModule.GetSyncOffset: Integer;
begin
  Result := (MainConnection as IBaseDBConnection).GetSyncOffset;
end;

procedure TAbstractDBModule.SetSyncOffset(AValue: Integer);
begin
  (MainConnection as IBaseDBConnection).SetSyncOffset(AValue);
end;

function TAbstractDBModule.GetConnection: TAbstractDBConnection;
var
  aClass: TComponentClass;
begin
  if not Assigned(FMainConnection) then
    begin
      aClass := GetConnectionClass;
      FMainConnection := TAbstractDBConnection(aClass.Create(Self));
    end;
  Result := FMainConnection;
end;

function TAbstractDBModule.GetLimitAfterSelect: Boolean;
begin
  Result := (MainConnection as IBaseDBConnection).GetLimitAfterSelect;
end;

function TAbstractDBModule.GetLimitSTMT: string;
begin
  Result := (MainConnection as IBaseDBConnection).GetLimitSTMT;
end;

function TAbstractDBModule.GetDataSetClass: TDatasetClass;
begin
  Result := QueryClass;
end;

function TAbstractDBModule.GetConnectionClass: TComponentClass;
begin
  Result := ConnectionClass;
end;

constructor TAbstractDBModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUseParameters := True;
  FTables := TStringList.Create;
  FTriggers := TStringList.Create;
  FCheckedTables := TStringList.Create;
  FCS := TCriticalSection.Create;
  FIgnoreOpenrequests := False;
  FUseExtData:=False;
  FUseParameters:=True;
end;

destructor TAbstractDBModule.Destroy;
begin
  IgnoreOpenRequests:=True;
  FreeAndNil(FMainConnection);
  FTables.Free;
  FTriggers.Free;
  inherited Destroy;
  FCheckedTables.Free;
  FCS.Destroy;
end;

function TAbstractDBModule.GetNewDataSet(aTable: TAbstractDBDataset;
  aConnection: TComponent; MasterData: TDataSet; aTables: string): TDataSet;
begin
  //if IgnoreOpenrequests then exit;
  Result := GetDataSetClass.Create(Self);
  if not Assigned(aConnection) then
    aConnection := MainConnection;
  with TAbstractDBQuery(Result) as IBaseManageDB do
    begin
      SetConnection(aConnection);
      SetTableNames(aTables);
      aTable.DefineFields(Result);
      aTable.DefineDefaultFields(Result,Assigned(Masterdata));
      SetOrigTable(aTable);
      if Assigned(Masterdata) then
        begin
          if not Assigned((MasterData as IBaseManageDB).MasterSource) then
            begin
              (MasterData as IBaseManageDB).MasterSource := TDataSource.Create(Self);
              (MasterData as IBaseManageDB).MasterSource.DataSet := MasterData;
            end;
          DataSource := (MasterData as IBaseManageDB).MasterSource;
          with Masterdata as IBaseSubDataSets do
            RegisterSubDataSet(aTable);
        end;
    end;
end;

function TAbstractDBModule.GetNewDataSet(aSQL: string; aConnection: TComponent;
  MasterData: TDataSet; aOrigtable: TAbstractDBDataset): TDataSet;
var
  aClass: TDatasetClass;
begin
  aClass := GetDataSetClass;
  Result := aClass.Create(Self);
  if not Assigned(aConnection) then
    aConnection := MainConnection;
  with TAbstractDBQuery(Result) as IBaseManageDB,TAbstractDBQuery(Result) as IBaseDbFilter do
    begin
      SetOrigTable(aOrigtable);
      SetConnection(aConnection);
      FullSQL := aSQL;
      if Assigned(Masterdata) then
        begin
          if not Assigned((MasterData as IBaseManageDB).MasterSource) then
            begin
              (MasterData as IBaseManageDB).MasterSource := TDataSource.Create(Self);
              (MasterData as IBaseManageDB).MasterSource.DataSet := MasterData;
            end;
          DataSource := (MasterData as IBaseManageDB).MasterSource;
        end;
    end;
end;

function TAbstractDBModule.GetNewConnection(Connected: Boolean): TComponent;
begin
  Result := GetConnectionClass.Create(Self);
  with Result as IBaseDBConnection do
    begin
      if DoSetProperties(FProperties) and Connected then
        begin
          (Result as IBaseDBConnection).DoConnect;
          if ((Result as IBaseDBConnection).IsConnected) then
            (Result as IBaseDBConnection).DoInitializeConnection;
        end;
    end;
end;

function TAbstractDBModule.StartTransaction(aConnection: TComponent;
  ForceTransaction: Boolean): Boolean;
begin
  Result := (aConnection as IBaseDBConnection).DoStartTransaction(ForceTransaction);
end;

function TAbstractDBModule.CommitTransaction(aConnection: TComponent): Boolean;
begin
  Result := (aConnection as IBaseDBConnection).DoCommitTransaction;
end;

function TAbstractDBModule.RollbackTransaction(aConnection: TComponent
  ): Boolean;
begin
  Result := (aConnection as IBaseDBConnection).DoRollbackTransaction;
end;

function TAbstractDBModule.IsTransactionActive(aConnection: TComponent
  ): Boolean;
begin
  Result := (aConnection as IBaseDBConnection).IsTransactionActive;
end;

function TAbstractDBModule.Ping(aConnection: TComponent): Boolean;
begin
  Result := True;
end;

function TAbstractDBModule.ShouldCheckTable(aTableName: string;
  SetChecked: Boolean): Boolean;
begin
  Result := FCheckedTables.IndexOf(aTableName) = -1;
  if (aTableName='DBTABLES') or (not Result) then exit;
  if SetChecked and TableExists(aTableName) and (FCheckedTables.IndexOf(aTableName) = -1) then
    FCheckedTables.Add(aTableName);
end;

function TAbstractDBModule.RemoveCheckTable(aTableName: string): Boolean;
begin
  if FCheckedTables.IndexOf(aTableName) > -1 then
    FCheckedTables.Delete(FCheckedTables.IndexOf(aTableName));
  Tables.Clear;
end;

function TAbstractDBModule.SetProperties(aProp: string;
  Connection: TAbstractDBConnection): Boolean;
begin
  FProperties := aProp;
  if Connection=nil then
    Connection := MainConnection;
  Result := (Connection as IBaseDBConnection).DoSetProperties(aProp);
  FUseExtData:=(Connection as IBaseDBConnection).UseExtData;
  FDatabaseDir:=(Connection as IBaseDBConnection).GetDatabaseDir;
  if Result then
    begin
      (Connection as IBaseDBConnection).DoConnect;
      Result := Result and ((Connection as IBaseDBConnection).IsConnected) and (Connection as IBaseDBConnection).DoInitializeConnection;
    end;
  Tables.Clear;
  CheckedTables.Clear;
end;

function TAbstractDBModule.CreateDBFromProperties(aProp: string;
  Connection: TAbstractDBConnection): Boolean;
begin
  if Connection=nil then
    Connection := MainConnection;
  Result := (Connection as IBaseDBConnection).DoCreateDBFromProperties(aProp);
  (Connection as IBaseDBConnection).DoConnect;
  Result := Result and ((Connection as IBaseDBConnection).IsConnected) and (Connection as IBaseDBConnection).DoInitializeConnection;
  Tables.Clear;
  CheckedTables.Clear;
end;

function TAbstractDBModule.TableExists(aTableName: string;
  aConnection: TComponent; AllowLowercase: Boolean): Boolean;
var
  aIndex: longint;
  i: Integer;
  tmp: String;
  aQuerry: TAbstractDBQuery;
begin
  Result := False;
  if not (MainConnection as IBaseDBConnection).IsConnected then exit;
  aTableName:=GetFullTableName(aTableName);
  aTableName:=StringReplace(aTableName,copy(QuoteField(''),0,1),'',[rfReplaceAll]);
  try
    if Tables.Count = 0 then
      begin
        //Get uncached
        if not Assigned(aConnection) then
          aConnection := MainConnection;
        (aConnection as IBaseDBConnection).DoGetTableNames(Tables);
        (aConnection as IBaseDBConnection).DoGetTriggerNames(Triggers);
      end;
  except
  end;
  if Tables.IndexOf(aTableName) > 0 then
    begin
      Result := True;
      exit;
    end;
  for i := 0 to Tables.Count-1 do
    begin
      tmp := Tables[i];
      if (Uppercase(tmp) = aTableName)
      then
        begin
          Result := True;
          break;
        end;
    end;
  //Try to open the non existent Table since we dont know if its in another database
  if not Result then
    begin
      aTableName:=GetFullTableName(aTableName);
      aQuerry := TAbstractDbQuery(GetNewDataSet('select count(*) from '+aTableName));
      if (GetDBType = 'mssql') then
        aQuerry.SQL := '('+aQuerry.SQL+')';
      try
        aQuerry.Open;
      except
      end;
      Result := aQuerry.Active;
      if Result then
        begin
          aTableName:=StringReplace(aTableName,copy(QuoteField(''),0,1),'',[rfReplaceAll]);
          if Tables.Count>0 then
            Tables.Add(aTableName);
        end;
      aQuerry.Free;
    end;
end;

function TAbstractDBModule.TriggerExists(aTriggerName: string;
  aConnection: TComponent; AllowLowercase: Boolean): Boolean;
begin
  Result := False;
end;

function TAbstractDBModule.ExecuteDirect(aSQL: string; aConnection: TComponent
  ): Integer;
begin
  if aConnection = nil then
    aConnection := MainConnection;
  Result := (aConnection as IBaseDBConnection).DoExecuteDirect(aSQL);
end;

function TAbstractDBModule.BuildSQL(DataSet: TDataSet): string;
var
  DoQuote : Boolean = False;

function BuildJoins : string;
var
  aDS : string;
  tmp: String;
begin
  if not (pos(',',(DataSet as IBaseManageDB).GetTableNames) > 0) then
    begin
      Result := (DataSet as IBaseManageDB).GetTableNames;
      if Result = '' then
        begin
          Result := GetFullTableName((DataSet as IBaseManageDB).TableName);
          DoQuote:=(pos('.',Result)>0) or DoQuote;
        end
      else Result := QuoteField(Result);
      exit;
    end;
  tmp := (DataSet as IBaseManageDB).GetTableNames+',';
  Result := copy((DataSet as IBaseManageDB).GetTableNames,0,pos(',',(DataSet as IBaseManageDB).GetTableNames)-1);
  aDS := Result;
  tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
  while pos(',',tmp) > 0 do
    begin
      Result := Result+ ' inner join '+QuoteField(copy(tmp,0,pos(',',tmp)-1))+' on '+QuoteField(copy(tmp,0,pos(',',tmp)-1))+'.'+QuoteField('REF_ID')+'='+aDS+'.'+QuoteField('SQL_ID');
      aDS := QuoteField(copy(tmp,0,pos(',',tmp)-1));
      tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
    end;
end;

var
  aFilter: String;
  aRefField: String;
  tmp: String;
  SResult: String;
  PJ: String = '';
  PW: String = '';

  procedure BuildSResult;
  begin
    SResult := '';
    if pos(',',QuoteField((DataSet as IBaseDbFilter).SortFields)) = 0 then
      begin
        sResult += QuoteField((DataSet as IBaseManageDB).TableName)+'.'+QuoteField((DataSet as IBaseDbFilter).SortFields);
        if (DataSet as IBaseDbFilter).SortDirection = sdAscending then
          sResult += ' ASC'
        else if (DataSet as IBaseDbFilter).SortDirection = sdDescending then
          sResult += ' DESC'
        else
          begin
            if (DataSet as IBaseDbFilter).BaseSortDirection = sdAscending then
              sResult += ' ASC'
            else if (DataSet as IBaseDbFilter).BaseSortDirection = sdDescending then
              sResult += ' DESC'
          end;
      end
    else
      begin
        tmp := (DataSet as IBaseDbFilter).SortFields;
        while pos(',',tmp) > 0 do
          begin
            sResult += QuoteField((DataSet as IBaseManageDB).TableName)+'.'+QuoteField(copy(tmp,0,pos(',',tmp)-1));
            tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
            if (DataSet as IBaseDbFilter).SortDirection = sdAscending then
              sResult += ' ASC'
            else
              sResult += ' DESC';
            if trim(tmp) > '' then
              sResult+=',';
          end;
        if tmp <> '' then
          begin
            sResult += QuoteField((DataSet as IBaseManageDB).TableName)+'.'+QuoteField(tmp);
            if (DataSet as IBaseDbFilter).SortDirection = sdAscending then
              sResult += ' ASC'
            else
              sResult += ' DESC';
          end;
      end;
  end;

begin
  if (DataSet as IBaseDbFilter).GetSQL <> '' then
    begin
      BuildSResult;
      if ((DataSet as IBaseManageDB).ManagedFieldDefs.IndexOf('AUTO_ID') = -1) and (UsersFilter <> '') and (DataSet as IBaseDbFilter).UsePermissions and TableExists('PERMISSIONS') then
        begin
          PJ := ' LEFT JOIN '+QuoteField('PERMISSIONS')+' ON ('+QuoteField('PERMISSIONS')+'.'+QuoteField('REF_ID_ID')+'='+QuoteField((DataSet as IBaseManageDB).TableName)+'.'+QuoteField('SQL_ID')+')';
          PW := ' AND ('+aFilter+') AND (('+UsersFilter+') OR '+QuoteField('PERMISSIONS')+'.'+QuoteField('USER')+' is NULL)';
        end
      else if ((DataSet as IBaseManageDB).ManagedFieldDefs.IndexOf('AUTO_ID') = -1) and (DataSet as IBaseDbFilter).UsePermissions and TableExists('PERMISSIONS') then
        begin
          PJ := ' LEFT JOIN '+QuoteField('PERMISSIONS')+' ON ('+QuoteField('PERMISSIONS')+'.'+QuoteField('REF_ID_ID')+'='+QuoteField((DataSet as IBaseManageDB).TableName)+'.'+QuoteField('SQL_ID')+')';
          PW := ' AND ('+QuoteField('PERMISSIONS')+'.'+QuoteField('USER')+' is NULL)'
        end;
      PW := StringReplace(PW,'AND ()','',[rfReplaceAll]);
      Result := StringReplace(StringReplace(StringReplace((DataSet as IBaseDbFilter).GetSQL,'@PERMISSIONJOIN@',PJ,[]),'@PERMISSIONWHERE@',PW,[]),'@DEFAULTORDER@',SResult,[]);
    end
  else if Assigned((DataSet as IBaseManageDB).GetOrigTable) then
    begin
      Result := 'SELECT ';
      if (DataSet as IBaseDbFilter).Distinct then
        Result := Result+'DISTINCT ';
      if LimitAfterSelect and (((DataSet as IBaseDbFilter).Limit > 0)) then
        begin
          if ParameteriseSQL then
            Result += Format(LimitSTMT,[':Limit'])+' '
          else
            Result += Format(LimitSTMT,[IntToStr((DataSet as IBaseDbFilter).Limit)])+' '
        end;
      if (DataSet as IBaseDbFilter).Fields = '' then
        Result += QuoteField((DataSet as IBaseManageDB).TableName)+'.'+'* '
      else
        Result += (DataSet as IBaseDbFilter).Fields+' ';
      aFilter := (DataSet as IBaseDbFilter).GetIntFilter;
      if ((DataSet as IBaseDbFilter).BaseFilter <> '') and (aFilter <> '') then
        aFilter := '('+(DataSet as IBaseDbFilter).BaseFilter+') and ('+aFilter+')'
      else if ((DataSet as IBaseDbFilter).BaseFilter <> '') then
        aFilter := '('+(DataSet as IBaseDbFilter).BaseFilter+')';
      if Assigned((DataSet as IBaseManageDB).DataSource) then
        begin
          with DataSet as IBaseManageDb do
            begin
              if ManagedFieldDefs.IndexOf('AUTO_ID') > -1 then
                aRefField := 'AUTO_ID'
              else
                aRefField := 'SQL_ID';
            end;
          if (aFilter <> '') and (pos('REF_ID',aFilter)=0) then
            aFilter := '('+aFilter+') and ('+QuoteField('REF_ID')+'=:'+QuoteField(aRefField)+')'
          else if (aFilter <> '') then //REF_ID in Filter so we use only the Filter
            aFilter := '('+aFilter+')'
          else
            aFilter := QuoteField('REF_ID')+'=:'+QuoteField(aRefField);
          if DataSet.FieldDefs.IndexOf('DELETED')>-1 then
            begin
              if aFilter <> '' then
                aFilter += ' AND ';
              aFilter += QuoteField('DELETED')+'<>'+QuoteValue('Y');
            end;
        end;
      if ((DataSet as IBaseManageDB).ManagedFieldDefs.IndexOf('AUTO_ID') = -1) and (UsersFilter <> '') and (DataSet as IBaseDbFilter).UsePermissions and TableExists('PERMISSIONS') then
        Result += 'FROM '+BuildJoins+' LEFT JOIN '+QuoteField('PERMISSIONS')+' ON ('+QuoteField('PERMISSIONS')+'.'+QuoteField('REF_ID_ID')+'='+QuoteField((DataSet as IBaseManageDB).TableName)+'.'+QuoteField('SQL_ID')+') WHERE ('+aFilter+') AND (('+UsersFilter+') OR '+QuoteField('PERMISSIONS')+'.'+QuoteField('USER')+' is NULL)'
      else if ((DataSet as IBaseManageDB).ManagedFieldDefs.IndexOf('AUTO_ID') = -1) and (DataSet as IBaseDbFilter).UsePermissions and TableExists('PERMISSIONS') then
        Result += 'FROM '+BuildJoins+' LEFT JOIN '+QuoteField('PERMISSIONS')+' ON ('+QuoteField('PERMISSIONS')+'.'+QuoteField('REF_ID_ID')+'='+QuoteField((DataSet as IBaseManageDB).TableName)+'.'+QuoteField('SQL_ID')+') WHERE ('+aFilter+') AND ('+QuoteField('PERMISSIONS')+'.'+QuoteField('USER')+' is NULL)'
      else
        Result += 'FROM '+BuildJoins+' WHERE ('+aFilter+')';
      Result := StringReplace(Result,' WHERE () AND ','WHERE ',[]);
      Result := StringReplace(Result,' WHERE ()','',[]);
      //if (copy(TZConnection(MainConnection).Protocol,0,5) = 'mssql') and DoQuote then
      //  Result := '('+Result+')';
      if ((DataSet as IBaseDbFilter).SortFields <> '') and (((DataSet as IBaseDbFilter).SortDirection <> sdIgnored) or ((DataSet as IBaseDbFilter).BaseSortDirection <> sdIgnored)) then
        begin
          BuildSResult;
          if (DataSet as IBaseDbFilter).UseBaseSorting then
            Result += ' ORDER BY '+Format((DataSet as IBaseDbFilter).BaseSorting,[sResult])
          else
            Result += ' ORDER BY '+sResult;
        end;
      if ((DataSet as IBaseDbFilter).Limit > 0) and (not LimitAfterSelect) then
        begin
          if ParameteriseSQL then
            Result += ' '+Format(LimitSTMT,[':Limit'])
          else
            Result += ' '+Format(LimitSTMT,[IntToStr((DataSet as IBaseDbFilter).Limit)]);
        end;
    end
  else
    Result := (DataSet as IBaseDbFilter).GetSQL;
  //LastStatement := Result;
end;

function TAbstractDBModule.GetUniID(aConnection: TComponent; Generator: string;
  Tablename: string; AutoInc: Boolean): Variant;
begin
  if aConnection = nil then
    aConnection := MainConnection;
  Result := (aConnection as IBaseDBConnection).GetUniID(aConnection,Generator,Tablename,AutoInc);
end;

function TAbstractDBModule.DateToFilter(aValue: TDateTime): string;
begin
  if GetDBType = 'mssql' then
    Result := QuoteValue(FormatDateTime('YYYYMMDD',aValue))
  else
    Result := QuoteValue(FormatDateTime('YYYY-MM-DD',aValue));
end;
function TAbstractDBModule.DateTimeToFilter(aValue: TDateTime): string;
begin
  if aValue>MaxDateTime then
    aValue:=MaxDateTime;
  if GetDBType = 'mssql' then
    Result := QuoteValue(FormatDateTime('YYYYMMDD HH:MM:SS.ZZZZ',aValue))
  else
    Result := QuoteValue(FormatDateTime('YYYY-MM-DD HH:MM:SS',aValue));
end;
function TAbstractDBModule.QuoteField(aField: string): string;
begin
  if GetDBType = 'mysql' then
    Result := '`'+aField+'`'
  else
    Result := '"'+aField+'"';
end;
function TAbstractDBModule.QuoteValue(aValue: string): string;
begin
  Result := ''''+StringReplace(aValue,'''','''''',[rfReplaceAll])+'''';
end;
function TAbstractDBModule.EscapeString(aValue: string): string;
begin
  Result := StringReplace(aValue,'''','',[rfReplaceAll]);
end;

function TAbstractDBModule.GetDBType: string;
begin
  Result := (MainConnection as IBaseDBConnection).DoGetDBLayerType;
end;

function TAbstractDBModule.GetDBLayerType: string;
begin
  Result := 'SQL';
end;

function TAbstractDBModule.FieldToSQL(aName: string; aType: TFieldType;
  aSize: Integer; aRequired: Boolean): string;
begin
  if aName <> '' then
    Result := Self.QuoteField(aName);
  case aType of
  ftString:
    begin
      if (GetDBType = 'firebird')
      or (GetDBType = 'postgres')
      then
        Result := Result+' VARCHAR('+IntToStr(aSize)+')'
      else
        Result := Result+' NVARCHAR('+IntToStr(aSize)+')';
    end;
  ftSmallint,
  ftInteger:Result := Result+' INTEGER';
  ftLargeInt:
    begin
      Result := Result+' BIGINT';
    end;
  ftAutoInc:
    begin
      if (GetDBType = 'mssql') then
        Result := Result+' INTEGER PRIMARY KEY IDENTITY'
      else if (GetDBType = 'sqlite') then
        Result := Result+' INTEGER PRIMARY KEY AUTOINCREMENT'
      else Result := Result+' INTEGER PRIMARY KEY';
    end;
  ftFloat:
    begin
      if (GetDBType = 'firebird') then
        Result := Result+' DOUBLE PRECISION'
      else
        Result := Result+' FLOAT';
    end;
  ftDate:
    begin
      if (GetDBType = 'mssql') then
        Result := Result+' DATETIME'
      else
        Result := Result+' DATE';
    end;
  ftDateTime:
    begin
      if (GetDBType = 'mssql')
      or (GetDBType = 'mysql')
      or (GetDBType = 'sqlite')
      then
        Result := Result+' DATETIME'
      else
        Result := Result+' TIMESTAMP'
    end;
  ftTime:
    begin
      if (GetDBType = 'mssql') then
        Result := Result+' DATETIME'
      else
        Result := Result+' TIME';
    end;
  ftBlob:
    begin
      if (GetDBType = 'mssql') then
        Result := Result+' IMAGE'
      else if (GetDBType = 'postgres') then
        Result := Result+' BYTEA'
      else if (GetDBType = 'mysql') then
        Result := Result+' LONGBLOB'
      else
        Result := Result+' BLOB';
    end;
  ftMemo:
    begin;
      if (GetDBType = 'firebird') then
        Result := Result+' BLOB SUB_TYPE 1'
      else if (GetDBType = 'mysql') then
        Result := Result+' LONGTEXT'
      else
        Result := Result+' TEXT';
    end;
  end;
  if aRequired then
    Result := Result+' NOT NULL'
  else
    begin
      if (GetDBType = 'mssql') then
        Result := Result+' NULL'
    end;
end;

function TAbstractDBModule.GetColumns(aTableName: string): TStrings;
begin
  Result := (MainConnection as IBaseDBConnection).DoGetColumns(aTableName);
end;

function TAbstractDBModule.DecodeFilter(aSQL: string; Parameters: TStringList;
  var NewSQL: string): Boolean;
var
  aQuotes: String;
  i: Integer;
  aParamCont: String;
begin
  aQuotes := QuoteValue('');
  aQuotes := copy(aQuotes,0,1);
  i := 0;
  NewSQL := '';
  Parameters.Clear;
  if FUseParameters then
    begin
      while pos(aQuotes,aSQL)>0 do
        begin
          NewSQL:=NewSQL+copy(aSQL,0,pos(aQuotes,aSQL)-1);
          aSQL := copy(aSQL,pos(aQuotes,aSQL)+1,length(aSQL));
          NewSQL:=NewSQL+':Param'+IntToStr(i);
          aParamCont := copy(aSQL,0,pos(aQuotes,aSQL)-1);
          aSQL := copy(aSQL,pos(aQuotes,aSQL)+1,length(aSQL));
          if copy(aSQL,0,1)=aQuotes then
            begin
              aParamCont += aQuotes+copy(aSQL,0,pos(aQuotes,aSQL)-1);
              aSQL := copy(aSQL,pos(aQuotes,aSQL)+1,length(aSQL));
              aParamCont += aQuotes+copy(aSQL,0,pos(aQuotes,aSQL)-1);
              aSQL := copy(aSQL,pos(aQuotes,aSQL)+1,length(aSQL));
            end;
          Parameters.Values['Param'+IntToStr(i)]:=aParamCont;
          NewSQL:=NewSQL;
          inc(i);
        end;
      NewSQL:=NewSQL+aSQL;
    end
  else NewSQL:=aSQL;
end;

function TAbstractDBModule.CheckForInjection(aFilter: string): Boolean;
begin
  Result := False;
  if (pos('insert into',lowercase(aFilter)) > 0)
//  or (pos('update ',lowercase(aFilter)) > 0)
  or (pos('delete table',lowercase(aFilter)) > 0)
  or (pos('delete from',lowercase(aFilter)) > 0)
  or (pos('alter table',lowercase(aFilter)) > 0)
//  or (pos('union select ',lowercase(aFilter)) > 0)
  or (pos('select if ',lowercase(aFilter)) > 0)
  or (pos(' into outfile',lowercase(aFilter)) > 0)
  or (pos(' into dumpfile',lowercase(aFilter)) > 0)
  then
    begin
      raise Exception.Create(strSQLInjection);
      Result := True;
    end;
end;

function TAbstractDBModule.GetUSerCode: string;
begin
  Result := '';
end;

procedure TAbstractDBModule.DestroyDataSet(DataSet: TDataSet);
begin
  DataSet.Close;
end;

function TAbstractDBModule.BlobFieldToFile(DataSet: TDataSet; Fieldname: string;
  Filename: string; aSize: Integer): Boolean;
var
  fstream: TFileStream;
begin
  fstream := TFileStream.Create(UniToSys(Filename),fmCreate);
  try
    Result := BlobFieldToStream(DataSet,Fieldname,fstream,aSize);
  except
    fStream.Free;
    raise;
  end;
  fstream.Free;
end;
procedure TAbstractDBModule.FileToBlobField(Filename: string; DataSet: TDataSet;
  Fieldname: string);
var
  fstream: TFileStream;
begin
  fstream := TFileStream.Create(UniToSys(Filename),fmOpenRead);
  try
    StreamToBlobField(fstream,DataSet,Fieldname);
  except
    fstream.Free;
    raise;
  end;
  fstream.Free;
end;
const
  ChunkSize: Longint = 16384; { copy in 8K chunks }
procedure TAbstractDBModule.StreamToBlobField(Stream: TStream; aDataSet: TDataSet;
  Fieldname: string; Tablename: string);
var
  Edited: Boolean;
  dStream: TStream;
  pBuf    : Pointer;
  cnt: LongInt;
  totCnt: LongInt=0;
  DataSet: TDataSet;
  aFName: String;
  aFStream: TFileStream;
begin
  Edited := False;
  DataSet := aDataSet;
  if Tablename='' then
    TableName := (aDataSet as IBaseManageDB).TableName;
  if (DataSet.FieldDefs.IndexOf(Fieldname)=-1) then
    begin
      if DataSet.State=dsInsert then //get SQL_ID
        begin
          DataSet.Post;
          DataSet.Edit;
        end;
      if not ((FUseExtData and (Tablename='DOCUMENTS'))) then
        begin
          DataSet := GetNewDataSet('select '+QuoteField('SQL_ID')+','+QuoteField(Fieldname)+' from '+GetFullTableName((aDataSet as IBaseManageDB).TableName)+' where '+QuoteField('SQL_ID')+'='+QuoteValue(aDataSet.FieldByName('SQL_ID').AsString));
          DataSet.Open;
        end;
    end;
  //Save to File
  if (FUseExtData and (Tablename='DOCUMENTS')) then
    begin
      aFName := FDatabaseDir+DirectorySeparator+'edata'+DirectorySeparator;
      aFName:=aFName+Tablename+DirectorySeparator;
      if (DataSet.Fielddefs.IndexOf('TYPE')<>-1) then
        if DataSet.FieldByName('TYPE').AsString<>'' then
          aFName:=aFName+DataSet.FieldByName('TYPE').AsString+DirectorySeparator;
      aFName:=aFName+DataSet.FieldByName('SQL_ID').AsString+'.'+Fieldname+'.dat';
      ForceDirectories(ExtractFileDir(UniToSys(aFName)));
      aFStream := TFileStream.Create(UniToSys(aFName),fmCreate);
      aFStream.CopyFrom(Stream,0);
      aFStream.Free;
      if (DataSet.FieldDefs.IndexOf(Fieldname)>-1) and (not (DataSet.FieldByName(Fieldname).IsNull)) then
        begin
          Edited := DataSet.State=dsBrowse;
          if Edited then
            DataSet.Edit;
          DataSet.FieldByName(Fieldname).Clear;
          if Edited then
            DataSet.Post;
        end;
      exit;
    end;
  //Save to Database
  if (DataSet.State <> dsEdit) and (DataSet.State <> dsInsert) then
    begin
      DataSet.Edit;
      Edited := True;
    end;
  dStream := DataSet.CreateBlobStream(DataSet.FieldByName(Fieldname),bmWrite);
  try
    GetMem(pBuf, ChunkSize);
    try
      cnt := Stream.Read(pBuf^, ChunkSize);
      cnt := dStream.Write(pBuf^, cnt);
      totCnt := totCnt + cnt;
      {Loop the process of reading and writing}
      while (cnt > 0) do
        begin
          {Read bufSize bytes from source into the buffer}
          cnt := Stream.Read(pBuf^, ChunkSize);
          {Now write those bytes into destination}
          cnt := dStream.Write(pBuf^, cnt);
          {Increment totCnt for progress and do arithmetic to update the gauge}
          totcnt := totcnt + cnt;
        end;
    finally
      FreeMem(pBuf, ChunkSize);
    end;
  finally
    dStream.Free;
  end;
  if Edited then
    DataSet.Post;
  if DataSet<>aDataSet then
    DataSet.free;
end;
function TAbstractDBModule.BlobFieldToStream(aDataSet: TDataSet;
  Fieldname: string; dStream: TStream; aSize: Integer): Boolean;
var
  pBuf    : Pointer;
  cnt: LongInt;
  totCnt: LongInt=0;
  Stream: TStream = nil;
  DataSet: TDataSet;
  aFName: String;
  TableName: String;
begin
  Result := False;
  TableName := (aDataSet as IBaseManageDB).TableName;
  DataSet := aDataSet;
  if (FUseExtData and (Tablename='DOCUMENTS')) then
    begin
      aFName := FDatabaseDir+DirectorySeparator+'edata'+DirectorySeparator;
      aFName:=aFName+Tablename+DirectorySeparator;
      if (aDataSet.Fielddefs.IndexOf('TYPE')<>-1) then
        if aDataSet.FieldByName('TYPE').AsString<>'' then
          aFName:=aFName+aDataSet.FieldByName('TYPE').AsString+DirectorySeparator;
      aFName:=aFName+aDataSet.FieldByName('SQL_ID').AsString+'.'+Fieldname+'.dat';
      if (FileExists(UniToSys(aFName))) then
        begin
          Stream := TFileStream.Create(UniToSys(aFName),fmOpenRead);
        end;
    end;
  if not Assigned(Stream) then
    begin
      if DataSet.FieldDefs.IndexOf(Fieldname)=-1 then
        begin
          DataSet := GetNewDataSet('select '+QuoteField('SQL_ID')+','+QuoteField(Fieldname)+' from '+GetFullTableName((aDataSet as IBaseManageDB).TableName)+' where '+QuoteField('SQL_ID')+'='+QuoteValue(aDataSet.FieldByName('SQL_ID').AsString));
          DataSet.Open;
        end;
      Stream := BlobFieldStream(DataSet,Fieldname);
    end;
  if not Assigned(Stream) then exit;
  try
    try
    GetMem(pBuf, ChunkSize);
    try
      cnt := Stream.Read(pBuf^, ChunkSize);
      cnt := dStream.Write(pBuf^, cnt);
      totCnt := totCnt + cnt;
      {Loop the process of reading and writing}
      while (cnt > 0) do
        begin
          {Read bufSize bytes from source into the buffer}
          cnt := Stream.Read(pBuf^, ChunkSize);
          {Now write those bytes into destination}
          cnt := dStream.Write(pBuf^, cnt);
          {Increment totCnt for progress and do arithmetic to update the gauge}
          totcnt := totcnt + cnt;
          if aSize>-1 then
            if totCnt>aSize then break;
        end;
    finally
      FreeMem(pBuf, ChunkSize);
    end;
      Result := True;
    except
      begin
        Result := false;
        raise;
      end;
    end;
  finally
    Stream.Free;
  end;
  if DataSet<>aDataSet then
    DataSet.free;
end;

function TAbstractDBModule.BlobFieldStream(DataSet: TDataSet; Fieldname: string;
  Tablename: string): TStream;
var
  aField: TField;
begin
  aField := DataSet.FieldByName(Fieldname);
  Result := DataSet.CreateBlobStream(aField,bmRead);
end;


end.
