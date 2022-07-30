with Ada.Directories;

package body Dir_Iterators.Ancestor is
    package ASU renames Ada.Strings.Unbounded;

    function Iterate
       (Tree : Ancestor_Dir_Walk)
        return Ancestor_Dir_Iterator_Interfaces.Forward_Iterator'Class is
    begin
        return It : Ancestor_Dir_Iterator do
            It.Start := Tree.Start;
        end return;
    end Iterate;

    function Walk (Dir : String) return Ancestor_Dir_Walk is
    begin
        return ADW : Ancestor_Dir_Walk do
            ADW.Start := ASU.To_Unbounded_String (Dir);
        end return;
    end Walk;

    function Has_Element (Position : Cursor) return Boolean is
        Parent : ASU.Unbounded_String;
    begin
        pragma Unreferenced (Parent);
        Parent :=
           ASU.To_Unbounded_String
              (Ada.Directories.Containing_Directory
                  (ASU.To_String (Position.Current)));
        return True;
    exception
        when others =>
            return False;
    end Has_Element;

    overriding function First (Object : Ancestor_Dir_Iterator) return Cursor is
    begin
        return Cursor'(Current => Object.Start);
    end First;

    overriding function Next
       (It : Ancestor_Dir_Iterator; Position : Cursor) return Cursor is
    begin
        pragma Unreferenced (It);
        return
           Cursor'
              (Current =>
                  ASU.To_Unbounded_String
                     (Ada.Directories.Containing_Directory
                         (ASU.To_String(Position.Current))));
    end Next;

    function Element_Value
       (Tree : Ancestor_Dir_Walk; Position : Cursor) return String is
    begin
        pragma Unreferenced (Tree);
        return ASU.To_String(Position.Current);
    end Element_Value;

end Dir_Iterators.Ancestor;
