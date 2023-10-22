SELECT *
FROM SQL_DataExploration..CovidDeaths
WHERE continent IS NOT NULL
ORDER BY 3,4

-- SELECT *
-- FROM SQL_DataExploration..CovidVaccinations
-- ORDER BY 3,4

-- Select the needed Data
SELECT Location, date, total_cases, new_cases, total_deaths, population
FROM SQL_DataExploration..CovidDeaths
WHERE continent IS NOT NULL
ORDER BY 1,2


-- Checking the total cases vs total deaths 
-- Shows the likelihood of dying if you contact covid in a certain country
SELECT Location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 AS DeathRatio
FROM SQL_DataExploration..CovidDeaths
WHERE location like '%Netherlands%'
ORDER BY 1,2

-- Checking the total cases vs population
-- Shows the percentage of the total population infected with Covid

SELECT Location, date, population, total_cases, (total_cases/population)*100 AS InfectionRatio
FROM SQL_DataExploration..CovidDeaths
WHERE location like '%Netherlands%'
ORDER BY 1,2

-- Checking which countries have the highest infection rate with regards to the population
SELECT Location, Population, MAX(total_cases) AS HighestInfectionCount, MAX((total_cases/population))*100 AS InfectionRatio
FROM SQL_DataExploration..CovidDeaths
GROUP BY Location, Population
Order BY InfectionRatio DESC




-- Checking the continents with the highest death count per population
SELECT continent, MAX(cast(total_deaths as int)) AS TotalDeathCount
FROM SQL_DataExploration..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY continent
Order BY TotalDeathCount DESC


-- Global numbers

SELECT date, SUM(new_cases) AS total_cases, SUM(cast(new_deaths as int)) AS total_deaths, SUM(cast(new_deaths as int))/SUM(new_cases) *100  AS DeathPercentage
FROM SQL_DataExploration..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY date
ORDER BY 1,2


-- Joining tables and checking the total population vs vaccinations

-- Version 1: Use CTE
WITH PopvsVac (Continent, Location, Date, Population, New_Vaccinations, RollingPeopleVaccinated)
as 
(
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(CONVERT(int, vac.new_vaccinations)) OVER (Partition by dea.location ORDER BY 
dea.location, dea.date) AS RollingPeopleVaccinated
FROM SQL_DataExploration..CovidDeaths dea
JOIN SQL_DataExploration..CovidVaccinations vac
	ON dea.location = vac.location
	and dea.date = vac.date
WHERE dea.continent IS NOT NULL
--ORDER BY 2,3
)
SELECT *, (RollingPeopleVaccinated/Population)*100
FROM PopvsVac

-- Version 2: Temp Table

DROP TABLE IF exists #PercentPopulationVaccinated
CREATE TABLE #PercentPopulationVaccinated
(
Continent nvarchar(255),
Location nvarchar(255),
Date datetime,
Population numeric,
New_Vaccinations numeric,
RollingPeopleVaccinated numeric
)
INSERT INTO #PercentPopulationVaccinated
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(CONVERT(int, vac.new_vaccinations)) OVER (Partition by dea.location ORDER BY 
dea.location, dea.date) AS RollingPeopleVaccinated
FROM SQL_DataExploration..CovidDeaths dea
JOIN SQL_DataExploration..CovidVaccinations vac
	ON dea.location = vac.location
	and dea.date = vac.date
WHERE dea.continent IS NOT NULL
--ORDER BY 2,3

SELECT *, (RollingPeopleVaccinated/Population)*100
FROM #PercentPopulationVaccinated



-- Creating View to store data for visualization
Create View PercentPopulationVaccinated as
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(CONVERT(int, vac.new_vaccinations)) OVER (Partition by dea.location ORDER BY 
dea.location, dea.date) AS RollingPeopleVaccinated
FROM SQL_DataExploration..CovidDeaths dea
JOIN SQL_DataExploration..CovidVaccinations vac
	ON dea.location = vac.location
	and dea.date = vac.date
WHERE dea.continent IS NOT NULL
--ORDER BY 2,3

