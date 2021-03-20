data ParseTree = EmptyTree 
               | NodeProgramm ParseTree 
               | NodeDeclList ParseTree
               | NodeDeclListN ParseTree ParseTree
               | NodeDecl ParseTree
               deriving (Show, Read)


treeInsert :: ParseTree -> ParseTree -> ParseTree
treeInsert node (NodeDeclListN l r) = 
    case node of
        NodeDecl x -> NodeDeclListN node r
        NodeDeclListN x y -> NodeDeclListN l node
treeInsert node (NodeDecl)



func1

func2
    treeInsert NodeVarDeclInit x func3

func3
    tr = NodeVarDeclId x y z
