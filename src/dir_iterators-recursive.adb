package body Dir_Iterators.Recursive is

    package AD renames Ada.Directories;
    package ASU renames Ada.Strings.Unbounded;

    ---------------------------------------------------------------------------
    -- Internals
    ---------------------------------------------------------------------------

    -- Ada.Directories.Hierarchical_File_Names is optional, so manually provide
    -- these.
    function Is_Current_Directory_Name (Dir : String) return Boolean is
       (Dir = ".");
    function Is_Parent_Directory_Name (Dir : String) return Boolean is
       (Dir = "..");

    -- Return true if the entry is "..".  The "." entry is used to find the
    -- starting directory and to report subdirectories immediately prior to
    -- their contents.  However, the ".." entry is to the parent, so we need
    -- to detect it to prevent infinite recursion.  The actual entry of a
    -- directory needs to be skipped as well, because its "." version will
    -- cause it to be reported.
    function Should_Skip
       (Dir_Entry : AD.Directory_Entry_Type) return Boolean is
        Name : constant String := AD.Simple_Name (Dir_Entry);
        use type AD.File_Kind;
    begin
        return
           Is_Parent_Directory_Name (Name)
           or else
           (AD.Kind (Dir_Entry) = AD.Directory
            and then not Is_Current_Directory_Name (Name));
    end Should_Skip;

    -- Convenience override with a precondition.
    procedure Get_Next_Entry (It : in out Recursive_Dir_Iterator) with
        Inline,
        Pre => AD.More_Entries (It.Current_Search)
    is
    begin
        AD.Get_Next_Entry (It.Current_Search, It.Next_Entry);
    end Get_Next_Entry;

    --
    function Is_Current_Dir_Done
       (It : Recursive_Dir_Iterator) return Boolean is
       (not AD.More_Entries (It.Current_Search));

    -- Moves to the first entry which isn't the current or parent directory.
    -- Returns false if reaches the end of the current directory being
    -- iterated.
    function Next_In_Dir
       (It : in out Recursive_Dir_Iterator) return Boolean with
        Post =>
        (if AD.More_Entries (It.Current_Search) then It.Has_Valid_Entry)
    is
        use type AD.File_Kind;
    begin
        while not Is_Current_Dir_Done (It) loop
            Get_Next_Entry (It);

            It.Has_Valid_Entry := not Should_Skip (It.Next_Entry);

            if not It.Has_Valid_Entry
               and then (It.Filter = null or else It.Filter(It.Next_Entry))
               and then AD.Kind (It.Next_Entry) = AD.Directory
               and then not Is_Parent_Directory_Name
                  (AD.Simple_Name (It.Next_Entry))
               and then not Is_Current_Directory_Name
                  (AD.Simple_Name (It.Next_Entry))
            then
                It.Current_Level.Append
                   (ASU.To_Unbounded_String (AD.Full_Name (It.Next_Entry)));
            end if;

            if It.Has_Valid_Entry then
                return True;
            end if;
        end loop;

        It.Has_Valid_Entry := False;
        return False;
    end Next_In_Dir;

    procedure Start_Search_In_Dir
       (It : in out Recursive_Dir_Iterator; Dir : String) is
        Filter : constant AD.Filter_Type :=
           (AD.Ordinary_File | AD.Directory => True, others => False);
    begin
        AD.Start_Search
           (Search => It.Current_Search, Directory => Dir, Pattern => "*",
            Filter => Filter);
    end Start_Search_In_Dir;

    function Start (Dir : Recursive_Dir_Walk) return Recursive_Dir_Iterator is
        -- Initializes the walk.  Note that `Done` might be true if there is
        -- nothing to walk.
        --
        -- TODO: Check for thrown error
        Has_Next : Boolean;
        Root_Dir : constant String := ASU.To_String(Dir.Root);
    begin
        pragma Unreferenced (Has_Next);
        return It : Recursive_Dir_Iterator (Dir.Filter) do
            Start_Search_In_Dir (It, Root_Dir);
            Has_Next := Next_In_Dir (It);
        end return;
    end Start;

    procedure Next (It : in out Recursive_Dir_Iterator) is
        package ASU renames Ada.Strings.Unbounded;
    begin
        -- Make forward progress if possible.
        if Next_In_Dir (It) then
            return;
        end if;

        -- We're out of entries, so move onto the next depth.
        while Is_Current_Dir_Done (It) loop
            -- Add the running list of the last directories content to front
            -- to be processed first.
            It.Left_To_Process.Prepend (It.Current_Level);
            It.Current_Level.Clear;

            if It.Left_To_Process.Is_Empty then
                -- Search is done!
                return;
            end if;

            -- No End_Search is needed here since the search will be finalized
            -- internally before being reused.

            Start_Search_In_Dir
               (It, ASU.To_String (It.Left_To_Process.First_Element));
            It.Left_To_Process.Delete_First;

            if Next_In_Dir (It) then
                return;
            end if;
        end loop;
    end Next;

    function Done (It : Recursive_Dir_Iterator) return Boolean is
    begin
        return
           not It.Has_Valid_Entry
           and then not AD.More_Entries (It.Current_Search)
           and then It.Left_To_Process.Is_Empty
           and then It.Current_Level.Is_Empty;
    end Done;

    function Iterate
       (Tree : Recursive_Dir_Walk)
        return Recursive_Dir_Iterator_Interfaces.Forward_Iterator'Class is
    begin
        return Start (Tree);
    end Iterate;

    function Walk (Dir : String; Filter : access function
       (Dir_Entry : Ada.Directories.Directory_Entry_Type)
                   return Boolean := null)
                   return Recursive_Dir_Walk is
    begin
        return RDT : Recursive_Dir_Walk (Filter) do
            RDT.Root := Ada.Strings.Unbounded.To_Unbounded_String (Dir);
        end return;
    end Walk;

    function Has_Element (Position : Cursor) return Boolean is
    begin
        return not Done (Position.It.all);
    end Has_Element;

    overriding function First
       (Object : Recursive_Dir_Iterator) return Cursor is
    begin
        return Cursor'(It => Object'Unrestricted_Access);
    end First;

    overriding function Next
       (It : Recursive_Dir_Iterator; Position : Cursor) return Cursor is
    begin
        pragma Unreferenced (It);
        Next (Position.It.all);
        return Position;
    end Next;

   function Element_Value (Tree : Recursive_Dir_Walk; Position : Cursor) return Reference_Type is
        pragma Unreferenced (Tree);

        -- Workaround for a bug in GCC 10.3, which labels this as a
        -- dangling reference.
        -- https://github.com/gcc-mirror/gcc/commit/25b4c873d19ccdc7e9a333eab8b5ab8e29a35976
        Res : constant Reference_Type := Reference_Type'(Element => Position.It.Next_Entry'Access);
    begin
        return Res;
    end Element_Value;

    overriding
    procedure Finalize (It : in out Recursive_Dir_Iterator) is
    begin
        -- Close out the last search.
        AD.End_Search (It.Current_Search);
    end Finalize;

end Dir_Iterators.Recursive;
