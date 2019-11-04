open TestFramework;
open Library;

let connection_url = "postgresql://postgres:postgres@localhost:5432/db_name";

let pool = Ezpostgresql.Pool.create(~conninfo=connection_url, ~size=8, ());

let create_table = () => {
  let table_name = QueryBuilder.tname("authors");

  // create table with list of columns
  // column(string, db type, fromString, toString, additional options)
  // constraints
  // -> generate create table, select, insert with encode/decode functions

  QueryBuilder.(
    [
      cdef("id", Types.text, ~primaryKey=true),
      cdef("first", Types.text),
      cdef("last", Types.text),
      constraint_(unique([cname("first"), cname("last")])),
    ]
    |> createTable(table_name, ~ifNotExists=true)
  );
};

let select_table = () => {
  let table_name = QueryBuilder.tname("authors");

  QueryBuilder.(
    select(
      [e(col("id")), e(col("first")), e(col("last"))]
      |> from(table(table_name)),
    )
  );
};

let insert_table = () => {
  let table_name = QueryBuilder.tname("authors");

  RowEncode.(
    [
      ("1", "Stephen", "King"),
      ("2", "Jane", "Austen"),
      ("3", "Kurt", "Vonnegut"),
    ]
    |> insertMany(columns3("id", string, "first", string, "last", string))
    |> into(table_name)
  );
};

let identity = a => a;


module User = {
  type t = {
    id: string,
    first: string,
    last: string,
  }
}

module UsersTable =
  Table.Make({
    type t = User.t;
  
    let tname = "authors";
  
    let columns =
      QueryBuilder.(User.[
        ("id", t => string(t.id)),
        ("first", t => string(t.first)),
        ("last", t => string(t.last)),
      ]);
  });

let connection_url = "postgresql://postgres:postgres@localhost:5432/db_name";

let pool = Client.create_pool(~size=8, ~connection_url, ());

describe("QueryBuilder", ({test, _}) => {
  test("CREATE TABLE", ({expect}) => {
    let create_table_query =
      Sql.CreateTable(create_table()) |> Postgres.render;
    expect.string(create_table_query).toMatchSnapshot();
  });

  test("SELECT", ({expect}) => {
    let select_table_query = Sql.Select(select_table()) |> Postgres.render;
    expect.string(select_table_query).toMatchSnapshot();
  });

  test("creates table, inserts rows and selectes them", ({expect}) => {
    let Ok(_) =
      pool |> Client.create_table(~query=create_table()) |> Lwt_main.run;

    let query =
    [User.{id: "4", first: "Stephen", last: "King"}]
      |> UsersTable.insert_many;

    let Ok(_) = pool |> Client.insert(~query) |> Lwt_main.run;

    let Ok(result) =
      pool |> Client.select_all(~query=select_table()) |> Lwt_main.run;

    let get = (column, row) =>
      row |> List.find(((col, _)) => col == column) |> snd;

    let users =
      result
      |> Array.map(row => {
           User.{
             id: row |> get("id"),
             first: row |> get("last"),
             last: row |> get("first"),
           }
         });

    Console.log(users);

    expect.int(result |> Array.length).toBe(3);
  });
  // TODO: INSERT returning
});
