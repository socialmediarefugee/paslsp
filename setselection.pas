unit SetSelection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  lsp, basic;

// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#window_showMessage
type
  { TInactiveRegionParams }

  { TSetSelectionParams }

  TSetSelectionParams=class(TPersistent)
  private
    fUri:string;
    fAnchor:TPosition;
    fActive:TPosition;
  published
    property uri: string read fUri write fUri;
    {**
    * The position at which the selection starts.
    * This position might be before or after {@link Selection.active active}.
    *}
    property anchor:TPosition read fAnchor write fAnchor;
    {**
     * The position of the cursor.
     * This position might be before or after {@link Selection.anchor anchor}.
     *}
    property active:TPosition read fActive write fActive;
  public
    procedure AfterConstruction; override;
  end;


  { TInactiveRegionsNotification }
  { The  message notification is sent from a server to a client to ask
    the client to display a inactive region in the user interface. }

  TSetSelectionNotification = class(TNotificationMessage)
  public
    constructor Create(uri:string; anchor:TPosition;active:TPosition);
    destructor Destroy; override;
  end;

implementation

{ TSetSelectionParams }

procedure TSetSelectionParams.AfterConstruction;
begin
  inherited AfterConstruction;
  //self.fAnchor:=TPosition.Create;
  //self.fActive:=TPosition.Create;
end;

{ TShowMessageNotification }

constructor TSetSelectionNotification.Create(uri:string; anchor:TPosition;active:TPosition);
begin
  params := TSetSelectionParams.Create;
  TSetSelectionParams(params).uri:=uri;
  TSetSelectionParams(params).anchor:=anchor;
  TSetSelectionParams(params).active:=active;

  method := 'pasls/setSelection';
end;

destructor TSetSelectionNotification.Destroy;
begin
  params.Free;
  inherited;
end;

end.

