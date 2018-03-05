unit uAbstractDBLayer;

interface

uses
  Classes,db,uBaseDatasetInterfaces,sysutils;

type
  TAbstractDBConnection = class(TComponent);
  TAbstractDBQuery = class(TDataSet);//IBaseDBFilter,IBaseManageDB,IBaseSubDatasets,IBaseModifiedDS

  { TAbstractDBModule }

  TAbstractDBModule = class(TComponent)
  public
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
