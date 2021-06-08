with Ada.Directories.Hierarchical_File_Names;

package body File_Iterators is
    package AD renames Ada.Directories;
    package ADH renames Ada.Directories.Hierarchical_File_Names;
    package ASU renames Ada.Strings.Unbounded;

    ---------------------------------------------------------------------------
    -- Internals
    ---------------------------------------------------------------------------

    -- Return true if the entry is "." or "..".  This provides a mechanism
    -- to detect files with these file names since they lead to problems
    -- during recursion.
    function Is_Current_Or_Parent_Directory
        (Dir_Entry : AD.Directory_Entry_Type) return Boolean is
        Name : constant String := AD.Simple_Name (Dir_Entry);
    begin
        return
            ADH.Is_Parent_Directory_Name (Name)
            or else ADH.Is_Current_Directory_Name (Name);
    end Is_Current_Or_Parent_Directory;

    procedure Get_Next_Entry (It : in out Recursive_File_Iterator) with
        Inline,
        Pre => AD.More_Entries (It.Current_Search)
    is
    begin
        AD.Get_Next_Entry (It.Current_Search, It.Next_Entry);
    end Get_Next_Entry;

    function Is_Current_Dir_Done
        (It : Recursive_File_Iterator) return Boolean is
        (not AD.More_Entries (It.Current_Search));

    -- Moves to the first entry which isn't the current or parent directory.
    -- Returns false if reaches the end of the current directory being
    -- iterated.
    function Next_In_Dir
        (It : in out Recursive_File_Iterator) return Boolean with
        Post => (if AD.More_Entries (It.Current_Search) then It.Has_Valid_Entry)
    is
    begin
        while not Is_Current_Dir_Done (It) loop
            Get_Next_Entry (It);

            It.Has_Valid_Entry :=
                not Is_Current_Or_Parent_Directory (It.Next_Entry);
            if It.Has_Valid_Entry then
                return True;
            end if;
        end loop;

        It.Has_Valid_Entry := False;
        return False;
    end Next_In_Dir;

    -- Indicates that the search reached a new entry, and to update the search
    -- based on the information in the current entry.
    procedure Search_Reached (It : in out Recursive_File_Iterator) is
        use AD;
        Is_Dir : constant Boolean := AD.Kind (It.Next_Entry) = AD.Directory;
    begin
        if Is_Dir then
            It.Left_To_Process.Append
                (ASU.To_Unbounded_String (AD.Full_Name (It.Next_Entry)));
        end if;
    end Search_Reached;

    procedure Start_Search_In_Dir
        (It : in out Recursive_File_Iterator; Dir : String) is
        Filter : constant AD.Filter_Type :=
            (AD.Ordinary_File | AD.Directory => True, others => False);
    begin
        AD.Start_Search
            (Search => It.Current_Search, Directory => Dir, Pattern => "*",
             Filter => Filter);
    end Start_Search_In_Dir;

    function Start (Dir : String) return Recursive_File_Iterator is
        -- Initializes the walk.  Note that `Done` might be true if there is
        -- nothing to walk.
    begin
        -- TODO: Check for thrown error
        return It : Recursive_File_Iterator do
            Start_Search_In_Dir (It, Dir);
            if Next_In_Dir (It) then
                Search_Reached (It);
            end if;
        end return;
    end Start;

    procedure Next (It : in out Recursive_File_Iterator) is
        package ASU renames Ada.Strings.Unbounded;
    begin
        -- Make forward progress if possible.
        if Next_In_Dir (It) then
            Search_Reached (It);
            return;
        end if;

        -- We're out of entries, so move onto the next depth.
        while Is_Current_Dir_Done (It) loop
            if It.Left_To_Process.Is_Empty then
                -- Search is done!
                return;
            end if;

            -- Breadth search by default.
            Start_Search_In_Dir
                (It, ASU.To_String (It.Left_To_Process.First_Element));
            It.Left_To_Process.Delete_First;

            if Next_In_Dir (It) then
                Search_Reached (It);
                return;
            end if;
        end loop;
    end Next;

    function Done (It : Recursive_File_Iterator) return Boolean is
    begin
        return
            not It.Has_Valid_Entry
            and then not AD.More_Entries (It.Current_Search)
            and then It.Left_To_Process.Is_Empty;
    end Done;

    function Iterate
        (Tree : Recursive_Dir_Tree)
         return Recursive_File_Iterator_Interfaces.Forward_Iterator'Class is
    begin
        return Start (ASU.To_String (Tree.Root));
    end Iterate;

    function Walk (Dir : String) return Recursive_Dir_Tree is
    begin
        return RDT : Recursive_Dir_Tree do
            RDT.Root := Ada.Strings.Unbounded.To_Unbounded_String (Dir);
        end return;
    end Walk;

    function Has_Element (Position : Cursor) return Boolean is
    begin
        return not Done (Position.It.all);
    end Has_Element;

    overriding function First
        (Object : Recursive_File_Iterator) return Cursor is
    begin
        return Cursor'(It => Object'Unrestricted_Access);
    end First;

    overriding function Next
        (It : Recursive_File_Iterator; Position : Cursor) return Cursor is
    begin
        Next (Position.It.all);
        return Position;
    end Next;

    function Element_Value
        (Tree : Recursive_Dir_Tree; Position : Cursor) return Reference_Type is
    begin
        pragma Unreferenced (Tree);
        return Reference_Type'(Element => Position.It.Next_Entry'Access);
    end Element_Value;

end File_Iterators;
