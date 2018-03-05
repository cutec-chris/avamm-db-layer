unit uAbstractDBLayer;

interface

uses
  Classes,db,uBaseDatasetInterfaces,sysutils;

type
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MainConnection : TAbstractDBConnection read GetConnection;

    property CheckedTables : TStrings read FCheckedTables;
    function ShouldCheckTable(aTableName : string;SetChecked : Boolean = False) : Boolean;
    function RemoveCheckTable(aTableName : string) : Boolean;
    property Tables : TStrings read FTables;
    property Triggers : TStrings read FTriggers;
    function DBExists : Boolean;
    function GetFullTableName(aTable: string): string;virtual;

    function SetProperties(aProp : string;Connection : TAbstractDBConnection = nil) : Boolean;virtual;
    function TableExists(aTableName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;virtual;
    function TriggerExists(aTriggerName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;virtual;

    function ExecuteDirect(aSQL: string; aConnection: TComponent=nil): Integer;virtual;

    function DateToFilter(aValue : TDateTime) : string;virtual;
    function DateTimeToFilter(aValue : TDateTime) : string;virtual;
    function QuoteField(aField : string) : string;virtual;
    function QuoteValue(aValue : string) : string;virtual;
    function EscapeString(aValue : string) : string;virtual;
    function GetDBType : string;virtual;abstract;
  end;

var
  QueryClass : TDatasetClass;
  ConnectionClass : TComponentClass;
implementation

{ TAbstractDBModule }

function TAbstractDBModule.DBExists: Boolean;
begin
  Result := TableExists('USERS') and TableExists('GEN_SQL_ID') and TableExists('GEN_AUTO_ID');
end;

function TAbstractDBModule.GetFullTableName(aTable: string): string;
begin
  Result := aTable;
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


end.
