# Automated Valid Query Generator For GraphQL

Project for randomly generating GraphQL queries based on the schema.

#### Using technologies:

![](./images/graphql.png)

<br>

![](./images/haskell.png)

## How to Use

Follow the steps below to download or clone the repository and execute the commands.

### Download or Clone the Repository

To get started, download or clone the repository to your local machine using the following command:

```bash
$ git clone <repository-url>
```


### Access the `src` Folder

Navigate to the `src` folder by running the following command:

```bash
$ cd src
```


### Generate a Query using Stack

To generate a query using Stack, run the following commands:

```bash
$ stack ghci GraphQLQueryGenerator.hs
```
Once inside the GHCi (Glasgow Haskell Compiler interactive) environment, execute the exportRandomQuery function to generate the query:

```bash
exportRandomQuery
```



### Execute Property-based Tests

To execute the property-based tests, use Stack and run the following command:

```bash
$ stack ghci GraphQLQueryTest.hs
```
Finally, run the `main` function to execute the tests:

```bash
main
```