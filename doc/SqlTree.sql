-- See http://www.codeproject.com/KB/database/Trees_in_SQL_databases.aspx
-- by Eugene Lepekhin

-- @doc Source code for tree hierarchies in MS SQL Server.
-- we can implement this hierarchy easily by adding a 'parent_id' to all resources (aka node)
doe 
if exists(select name from sysobjects where name = 'NodeInsert' and type = 'tr')
   drop trigger NodeInsert
go

if exists(select name from sysobjects where name = 'NodeUpdate' and type = 'tr')
   drop trigger NodeUpdate
go

if exists(select name from sysobjects where type = 'u' and name = 'Tree')
	Drop table Tree
go

if exists(select name from sysobjects where type = 'u' and name = 'Node')
	Drop table Node
go

create table Node(
	NodeId int not null,
	ParentId int null,
	NodeName varchar(255) not null,
	constraint PK_Node primary key(NodeId),
	constraint UK_NodeName unique(NodeName)
)
go

create table Tree(
	NodeId int not null,
	ParentId int not null,
	Level int not null,
	constraint PK_Tree primary key(NodeId, ParentId),
	constraint UK_Level unique(NodeId, Level)
)
go

alter table Node
	add constraint FK_NodeNode foreign key(ParentId) references Node(NodeId) --on delete cascade
go

alter table Tree
	add constraint FK_NodeTreeNode foreign key(NodeId) references Node(NodeId) on delete cascade
go

--alter table Tree
--	add constraint FK_NodeTreeParent foreign key(ParentId) references Node(NodeId) on delete cascade
--go

create trigger NodeInsert on Node for insert as
begin
	set nocount on

	insert into Tree(NodeId, ParentId, Level)
	select NodeId, NodeId, 0
	from inserted

	insert into Tree(NodeId, ParentId, Level)
	select n.NodeId, t.ParentId, t.Level + 1
	from inserted n, Tree t
	where n.ParentId = t.NodeId
end
go

create trigger NodeUpdate on Node for update as
if update(ParentId)
begin
	set nocount on

	declare @child table(NodeId int, Level int)

	insert into @child(NodeId, Level)
	select t.NodeId, t.Level
	from inserted n, Tree t
	where n.NodeId = t.ParentId and t.Level > 0

	delete Tree
	where
		Tree.NodeId in(select NodeId from @child)
		and Tree.ParentId in(
			select t.ParentId
			from inserted n, Tree t
			where n.NodeId = t.NodeId and t.Level > 0
		)

	delete Tree
	where Tree.NodeId in(select NodeId from inserted) and Tree.Level > 0

	insert into Tree(NodeId, ParentId, Level)
	select n.NodeId, t.ParentId, t.Level + 1
	from inserted n, Tree t
	where n.ParentId = t.NodeId

	insert into Tree(NodeId, ParentId, Level)
	select c.NodeId, t.ParentId, t.Level + c.Level
	from inserted n, Tree t, @child c
	where n.NodeId = t.NodeId and t.Level > 0
end
go

insert into Node(NodeId, ParentId, NodeName) values(1, null, 1)
insert into Node(NodeId, ParentId, NodeName) values(2, 1, 2)
insert into Node(NodeId, ParentId, NodeName) values(3, 1, 3)
insert into Node(NodeId, ParentId, NodeName) values(4, 2, 4)
insert into Node(NodeId, ParentId, NodeName) values(5, 4, 5)
insert into Node(NodeId, ParentId, NodeName) values(6, 4, 6)
insert into Node(NodeId, ParentId, NodeName) values(7, 6, 7)
select * from Node
select * from Tree

--gets all descendants of the Node 2
select c.*
from Node n, Tree t, Node c
where n.NodeName=2
	and n.NodeId = t.ParentId
	and t.NodeId = c.NodeId

--gets path to the root from node 7
select p.*
from Node n, Tree t, Node p
where n.NodeName=7
	and n.NodeId = t.NodeId
	and t.ParentId = p.NodeId

--changes parent of node 4 from 2 to 1
update Node set ParentId = 1 where NodeId = 4
select * from Node
select * from Tree
