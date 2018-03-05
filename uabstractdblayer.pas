unit uAbstractDBLayer;

interface

uses
  Classes,db,uBaseDatasetInterfaces,sysutils;

type

  { TInternalDBDataSet }

  TInternalDBDataSet = class
  private
    FDataSet: TDataSet;
  public
    destructor Destroy;override;
    property DataSet : TDataSet read FDataSet write FDataSet;
  end;

  TAbstractDBConnection = class(TComponent);//IBaseDBConnection
  TAbstractDBQuery = class(TDataSet);//IBaseDBFilter,IBaseManageDB,IBaseSubDatasets,IBaseModifiedDS

  { TAbstractDBModule }

  TAbstractDBModule = class(TComponent)
  private
    FCheckedTables : TStrings;
    FTables: TStrings;
    FTriggers: TStrings;
  protected
    function GetConnection: TAbstractDBConnection;virtual;abstract;
    function GetLimitAfterSelect: Boolean;virtual;
    function GetLimitSTMT: string;virtual;
    function GetDataSetClass : TDatasetClass;virtual;abstract;
    function GetConnectionClass : TComponentClass;virtual;abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MainConnection : TAbstractDBConnection read GetConnection;

    function GetNewDataSet(aTable : TAbstractDBDataset;aConnection : TComponent = nil;MasterData : TDataSet = nil;aTables : string = '') : TDataSet;virtual;
    function GetNewDataSet(aSQL : string;aConnection : TComponent = nil;MasterData : TDataSet = nil;aOrigtable : TAbstractDBDataset = nil) : TDataSet;virtual;

    property CheckedTables : TStrings read FCheckedTables;
    function ShouldCheckTable(aTableName : string;SetChecked : Boolean = False) : Boolean;
    function RemoveCheckTable(aTableName : string) : Boolean;
    property Tables : TStrings read FTables;
    property Triggers : TStrings read FTriggers;
    function DBExists : Boolean;
    function GetFullTableName(aTable: string): string;virtual;
    property LimitAfterSelect : Boolean read GetLimitAfterSelect;
    property LimitSTMT : string read GetLimitSTMT;

    function SetProperties(aProp : string;Connection : TAbstractDBConnection = nil) : Boolean;virtual;
    function CreateDBFromProperties(aProp : string;Connection : TAbstractDBConnection = nil) : Boolean;virtual;
    function TableExists(aTableName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;virtual;
    function TriggerExists(aTriggerName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;virtual;

    function ExecuteDirect(aSQL: string; aConnection: TComponent=nil): Integer;virtual;

    function DateToFilter(aValue : TDateTime) : string;virtual;
    function DateTimeToFilter(aValue : TDateTime) : string;virtual;
    function QuoteField(aField : string) : string;virtual;
    function QuoteValue(aValue : string) : string;virtual;
    function EscapeString(aValue : string) : string;virtual;
    function GetDBType : string;virtual;
    function GetDBLayerType : string;virtual;
    function FieldToSQL(aName : string;aType : TFieldType;aSize : Integer;aRequired : Boolean) : string;
    function GetColumns(aTableName : string) : TStrings;
  end;

var
  QueryClass : TDatasetClass;
  ConnectionClass : TComponentClass;
implementation

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

function TAbstractDBModule.GetFullTableName(aTable: string): string;
begin
  Result := aTable;
end;

function TAbstractDBModule.GetLimitAfterSelect: Boolean;
begin
  Result := (MainConnection as IBaseDBConnection).GetLimitAfterSelect;
end;

function TAbstractDBModule.GetLimitSTMT: string;
begin
  Result := (MainConnection as IBaseDBConnection).GetLimitSTMT;
end;

constructor TAbstractDBModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTables := TStringList.Create;
  FTriggers := TStringList.Create;
  FCheckedTables := TStringList.Create;
end;

destructor TAbstractDBModule.Destroy;
begin
  inherited Destroy;
  FCheckedTables.Free;
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
          MasterSource := (MasterData as IBaseManageDB).MasterSource;
          with Masterdata as IBaseSubDataSets do
            RegisterSubDataSet(aTable);
        end;
    end;
end;

function TAbstractDBModule.GetNewDataSet(aSQL: string; aConnection: TComponent;
  MasterData: TDataSet; aOrigtable: TAbstractDBDataset): TDataSet;
begin
  Result := GetDataSetClass.Create(Self);
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
          MasterSource := (MasterData as IBaseManageDB).MasterSource;
        end;
    end;
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
  if Connection=nil then
    Connection := MainConnection;
  Result := (Connection as IBaseDBConnection).DoSetProperties(aProp);
  (Connection as IBaseDBConnection).DoConnect;
  Result := Result and ((Connection as IBaseDBConnection).IsConnected) and (Connection as IBaseDBConnection).DoInitializeConnection;
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
      {
      aQuerry := TZReadOnlyQuery.Create(Self);
      aQuerry.Connection:=TZConnection(MainConnection);
      aQuerry.SQL.Text := 'select count(*) from '+aTableName;
      if (FMainConnection.Protocol = 'mssql') then
        aQuerry.SQL.Text := '('+aQuerry.SQL.Text+')';
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
      }
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
  Result := (aConnection as IBaseDBConnection).DoExecuteDirect(aSQL);
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
    Result := QuoteField(aName)
  else Result:='';
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
      else if (GetDBType = 'postgresql') then
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


end.
