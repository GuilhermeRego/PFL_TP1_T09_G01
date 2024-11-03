## Group Member Identification

- **Gabriel da Quinta Braga (up202207784)** - 50%
  - **Responsible for**: areAdjacent, adjacent, rome, travelSales

- **Guilherme Silveira Rego (up202207041)** - 50%
  - **Responsible for**: cities, distance, pathDistance, isStronglyConnected, shortestPath


## Implementation of the `shortestPath` Function

The `shortestPath` function aims to find the shortest path between two cities in a given `RoadMap`. The approach used is **Breadth-First Search (BFS)**, which allows exploring all possible paths from the starting city to the destination city.

### Justification for Data Structures

- **List of Paths (`[Path]`)**: A list is used to store all explored paths. Each path is represented as a list of cities. This structure is suitable as the list can grow dynamically as new paths are explored.
- **Minimum Distance (`minDist`)**: The `minimum` function combined with `map` is used to calculate the smallest distance among all found paths, facilitating the selection of the lowest-cost path.

### Algorithm

1. **Initial Condition Check**: If the starting city is the same as the destination city, return a list containing only that city.
2. **Path Exploration**: The `bfsPaths` function is called to find all possible paths between the two cities. For each found path, the distance is calculated using the `pathDistance` function.
3. **Filtering the Minimum Path**: The found paths are filtered based on the minimum distance, and those that match this distance are returned.

## Implementation of the `travelSales` Function

The `travelSales` function aims to find the shortest path that visits all cities in a `RoadMap`, starting and ending at the same city. To solve this problem, we utilize an approach that involves generating all permutations of the cities.

### Justification for Data Structures

- **City Permutations**: A list of lists (`[[Path]]`) is used to store all possible permutations of paths. The `permutations` function generates these lists recursively.
- **List of Valid Paths**: The permutations are filtered to find only the paths that have a valid distance (i.e., that can be traversed according to the connections in the `RoadMap`).

### Algorithm

1. **City Identification**: All cities are extracted from the `RoadMap`, and the first city is set as the starting point.
2. **Generating Valid Paths**: For each permutation of cities, a path is constructed that begins at the starting city. For each path, the distance is calculated using the `pathDistance` function, ensuring that the path ends at the starting city.
3. **Selecting the Shortest Path**: Based on the list of valid paths, the `shortestPath'` function is used to determine the path with the smallest distance.
