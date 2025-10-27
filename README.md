# 🏥 Hospital Transfer System

[![Prolog](https://img.shields.io/badge/Language-Prolog-blue.svg)](https://www.swi-prolog.org/)
[![License](https://img.shields.io/badge/License-Educational-green.svg)](LICENSE)
[![COU4303](https://img.shields.io/badge/Course-COU4303-purple.svg)]()

A comprehensive Prolog-based system for finding optimal patient transfer routes between hospitals using multiple pathfinding algorithms (DFS, BFS, A*) with traffic considerations and facility requirements.

## 📋 Table of Contents

- [Overview](#-overview)
- [Features](#-features)
- [System Architecture](#️-system-architecture)
- [Prerequisites](#-prerequisites)
- [Installation & Setup](#️-installation--setup)
- [Usage Guide](#-usage-guide)
- [Algorithm Details](#-algorithm-details)
- [Performance Metrics](#-performance-metrics)
- [Customization](#-customization)
- [Testing Scenarios](#-testing-scenarios)
- [Example Output](#-example-output)
- [Troubleshooting](#-troubleshooting)
- [Contributing](#-contributing)
- [License](#-license)

## 🏥 Overview

This system helps medical professionals find the best route to transfer patients between hospitals based on:
- **Medical facilities** available at each hospital
- **Road distances** and **traffic conditions**
- **Real-time road blockages**
- **Multiple pathfinding algorithms** for comparison

### 🎯 Key Benefits
- **Life-saving efficiency**: Find optimal routes for critical patient transfers
- **Traffic-aware routing**: Avoid congested roads for faster transfers
- **Facility matching**: Locate hospitals with required medical capabilities
- **Algorithm comparison**: Choose the best pathfinding strategy for each situation

## 🚀 Features

### Core Functionality
- **Multi-Algorithm Pathfinding**: DFS, BFS, and A* algorithms
- **Traffic-Aware Routing**: Considers low/medium/high traffic with cost multipliers
- **Facility-Based Destination Selection**: Finds hospitals with required medical facilities
- **Dynamic Road Management**: Block/unblock roads in real-time
- **Comprehensive Comparison**: Side-by-side algorithm performance analysis

### User Interfaces
- **Text-based Menu System**: Interactive command-line interface
- **XPCE GUI**: Optional graphical user interface (if XPCE is available)
- **Real-time Results**: Live algorithm comparison and cost analysis

## 🏗️ System Architecture

### Hospital Network
The system models 7 major hospitals across Sri Lanka:
- **Nawaloka Colombo** (0,0) - Advanced surgery, cardiac, neurology, dialysis
- **National Kandy** (80,80) - Advanced surgery, orthopedic, oncology, psychiatry  
- **Teaching Rathnapura** (-70,-30) - Advanced surgery, cardiac, dialysis
- **Asiri Galle** (-90,-90) - Advanced surgery, oncology, orthopedic
- **Kalmunai Ampara** (200,-50) - Advanced surgery, neurology, psychiatry
- **Venus Jaffna** (10,380) - Cardiac, orthopedic, neurology
- **Central Badulla** (160,-40) - Oncology, dialysis, psychiatry

### Traffic System
- **Low Traffic**: 1.0x cost multiplier
- **Medium Traffic**: 1.4x cost multiplier  
- **High Traffic**: 1.9x cost multiplier

### Available Facilities
- `advanced_surgery`
- `cardiac`
- `orthopedic` 
- `oncology`
- `dialysis`
- `psychiatry`
- `neurology`

## 📋 Prerequisites

- **SWI-Prolog** (tested version)
- **XPCE Library** (optional, for GUI functionality)
- **Standard Prolog libraries**: `lists`, `apply`, `pairs`

### Prerequisites
- **SWI-Prolog** (recommended version 8.0+)
- **XPCE Library** (optional, for GUI functionality)
- **Standard Prolog libraries**: `lists`, `apply`, `pairs`

### Quick Start
1. **Clone or download** the repository
2. **Rename the file** to `COU4303_GroupXX.pl` (replace XX with your group number)
3. **Load in SWI-Prolog**:
   ```prolog
   ?- [COU4303_GroupXX].
   ```
4. **Run the system**:
   ```prolog
   ?- run.    % Text menu interface
   ?- gui.    % GUI interface (if XPCE available)
   ```

### Alternative Setup Methods
- **Command line**: `swipl COU4303_GroupXX.pl`
- **IDE integration**: Load in Prolog IDE of choice

## 🎯 Usage Guide

### Text Menu Interface

#### 1. List Hospitals
View all hospitals with their coordinates and available facilities.

#### 2. Compare Algorithms
Compare BFS, DFS, and A* performance for a specific route:
- Select start and destination hospitals
- View path, cost, and nodes expanded for each algorithm

#### 3. Transfer (Nearest by Air)
Find the closest hospital with required facility:
- Select start hospital and required facility
- System finds nearest hospital by straight-line distance
- Compares all algorithms for the optimal route

#### 4. Transfer (Best by A*)
Find the most cost-effective hospital with required facility:
- Select start hospital and required facility  
- System uses A* to find most efficient route
- Compares all algorithms for the chosen destination

#### 5. Toggle Road Block
Block or unblock roads for testing:
- Select two connected hospitals
- Toggle road availability
- Useful for testing alternative routes

### GUI Interface

The graphical interface provides:
- **Dropdown menus** for hospital and facility selection
- **Algorithm selection** (nearest by air vs best by A*)
- **Real-time results** display
- **One-click comparison** functionality

## 🔍 Algorithm Details

### Depth-First Search (DFS)
- **Strategy**: Explores as far as possible along each branch
- **Characteristics**: May find suboptimal solutions, memory efficient
- **Use Case**: Quick exploration, limited memory scenarios

### Breadth-First Search (BFS)  
- **Strategy**: Explores all neighbors before moving to next level
- **Characteristics**: Guarantees shortest path (by number of hops)
- **Use Case**: When path length matters more than cost

### A* Search
- **Strategy**: Uses heuristic + actual cost for optimal pathfinding
- **Characteristics**: Most efficient, finds optimal solutions
- **Heuristic**: Straight-line distance between hospitals
- **Use Case**: Optimal routing with cost considerations

## 📊 Performance Metrics
he system tracks and displays comprehensive performance data:

| Metric | Description | Example |
|--------|-------------|---------|
| **Path** | Sequence of hospitals in the route | `[nawaloka_colombo, national_kandy]` |
| **Cost** | Total travel cost (distance × traffic multiplier) | `218.50` |
| **Expanded** | Number of nodes explored during search | `6` |

### Algorithm Comparison Results
- **A***: Usually finds optimal solutions with minimal expansions
- **BFS**: Guarantees shortest path by number of hops
- **DFS**: May find suboptimal solutions but memory efficient

## 🛡️ Error Handling

- **Invalid Input**: Prompts for valid hospital/facility names
- **No Route Found**: Gracefully handles unreachable destinations
- **Missing Facilities**: Informs when no hospital has required facility
- **Blocked Roads**: Automatically avoids blocked connections

## 🔧 Customization

### Adding New Hospitals
```prolog
hospital(name, [facilities], x_coord, y_coord).
```

### Adding New Roads
```prolog
road(hospital1, hospital2, distance).
traffic(hospital1, hospital2, level).  % low/medium/high
```

### Adding New Facilities
Update the facility list in:
- `ask_for_facility/1` predicate
- GUI facility dropdown
- `hospital_has_facility/2` validation

## 🧪 Testing Scenarios

### Basic Functionality
1. List all hospitals
2. Compare algorithms for direct routes
3. Find nearest facility for each medical specialty
4. Test with different start hospitals

### Edge Cases
1. Block major roads and test alternative routes
2. Test with hospitals that already have required facilities
3. Test unreachable destinations
4. Compare algorithm performance on long vs short routes

### Performance Analysis
1. Note A* typically finds optimal solutions with fewer expansions
2. Compare BFS vs DFS on different network topologies
3. Analyze traffic impact on routing decisions

## 📈 Example Output

### Algorithm Comparison
```prolog
=== Comparison nawaloka_colombo -> national_kandy ===
BFS : Path=[nawaloka_colombo,national_kandy]
      Cost=218.50, Expanded=6
DFS : Path=[nawaloka_colombo,national_kandy]  
      Cost=218.50, Expanded=1
A*  : Path=[nawaloka_colombo,national_kandy]
      Cost=218.50, Expanded=1  (usually best)
```

### Transfer Scenario
```prolog
Destination (nearest-air): national_kandy
=== Comparison nawaloka_colombo -> national_kandy ===
BFS : Path=[nawaloka_colombo,national_kandy]
      Cost=218.50, Expanded=6
DFS : Path=[nawaloka_colombo,national_kandy]  
      Cost=218.50, Expanded=1
A*  : Path=[nawaloka_colombo,national_kandy]
      Cost=218.50, Expanded=1  (usually best)
```

### Hospital Information
```prolog
nawaloka_colombo @ (0,0) -> Facilities: [advanced_surgery,cardiac,neurology,dialysis]
national_kandy @ (80,80) -> Facilities: [advanced_surgery,orthopedic,oncology,psychiatry]
```

## 🤝 Contributing

This is a COU4303 assignment project. For modifications:
1. Maintain the core algorithm implementations
2. Preserve the hospital network structure
3. Test all functionality after changes
4. Update documentation as needed

## 📝 License

Educational project for COU4303 course.

## 🆘 Troubleshooting
| Issue | Solution |
|-------|----------|
| **XPCE not available** | Use text menu interface (`?- run.`) instead of GUI |
| **No route found** | Check for blocked roads or disconnected hospitals |
| **Invalid input** | Ensure hospital/facility names match exactly |
| **File not found** | Verify file name matches `COU4303_GroupXX.pl` |
| **Syntax errors** | Check Prolog version compatibility |
### Debug Mode
Enable detailed output by modifying the `set_prolog_flag` settings:
```prolog
:-set_prolog_flag(answer_write_options,[max_depth(0)]).
```

### Getting Help
- Check SWI-Prolog documentation for library issues
- Verify XPCE installation for GUI functionality
- Test with simple queries first: `?- hospital(X,_,_,_).`

## 📚 Technical Details

### Code Structure
- **Hospital Data**: Facts defining hospitals, facilities, and coordinates
- **Road Network**: Bidirectional connections with distances and traffic
- **Algorithm Implementations**: DFS, BFS, and A* with proper data structures
- **GUI Integration**: XPCE-based interface with dropdown menus
- **Menu System**: Interactive text-based user interface

### Key Predicates
- `compare_all/2`: Main comparison function
- `do_transfer/3`: Transfer logic with facility matching
- `astar_path/5`: A* implementation with heuristic
- `gui_compare/3`: GUI event handling

---

## 📄 License

This project is created for educational purposes as part of COU4303 course requirements.

**Note**: This system demonstrates advanced Prolog programming concepts including graph algorithms, heuristic search, GUI development, and real-world problem solving in healthcare logistics.

---

## 👥 Team Members

This project was collaboratively developed by the **COU4303 group** as part of the course requirements at the Open University of Sri Lanka.

| Name | Role |
|------|------|
| Pathmanadan Dabositha | Logic & Development |
| Team Member 2 | Algorithm Implementation |
| Team Member 3 | GUI Design & Development |
| Team Member 4 | Testing & Documentation |
| Team Member 5 | Report & Presentation |
