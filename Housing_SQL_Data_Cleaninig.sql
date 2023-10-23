/*

Cleaning Data in SQL

*/

SELECT *
FROM SQL_DataCleaning..HousingProject

----------------------------------------------------------------------------------------------------------------

-- Standardize the Date Format

SELECT SaleDateConverted, CONVERT(Date, SaleDate)
FROM SQL_DataCleaning..HousingProject


ALTER TABLE SQL_DataCleaning..HousingProject
ADD SaleDateConverted Date;


UPDATE HousingProject
SET SaleDateConverted = CONVERT(Date, SaleDate)

----------------------------------------------------------------------------------------------------------------

-- Populate Property Address Data

SELECT *
FROM SQL_DataCleaning..HousingProject
--WHERE PropertyAddress IS NULL
ORDER BY ParcelID

SELECT a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress, b.PropertyAddress)
FROM SQL_DataCleaning..HousingProject a
JOIN SQL_DataCleaning..HousingProject b
	ON a.ParcelID = b.ParcelID
	AND a.[UniqueID] <> b.[UniqueID]
WHERE a.PropertyAddress IS NULL

UPDATE a
SET PropertyAddress = ISNULL(a.PropertyAddress, b.PropertyAddress)
FROM SQL_DataCleaning..HousingProject a
JOIN SQL_DataCleaning..HousingProject b
	ON a.ParcelID = b.ParcelID
	AND a.[UniqueID] <> b.[UniqueID]
WHERE a.PropertyAddress IS NULL


----------------------------------------------------------------------------------------------------------------

-- Breaking out the address into individual columns (Address, City, State)

SELECT PropertyAddress
FROM SQL_DataCleaning..HousingProject
--WHERE PropertyAddress IS NULL
--ORDER BY ParcelID

SELECT
SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) - 1) AS Address,
SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1, LEN(PropertyAddress)) AS City
FROM SQL_DataCleaning..HousingProject


ALTER TABLE SQL_DataCleaning..HousingProject
ADD PropertySplitAddress Nvarchar (255);
UPDATE SQL_DataCleaning..HousingProject
SET PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) - 1) 

ALTER TABLE SQL_DataCleaning..HousingProject
ADD PropertySplitCity Nvarchar (255);
UPDATE SQL_DataCleaning..HousingProject
SET PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1, LEN(PropertyAddress))



SELECT OwnerAddress
FROM SQL_DataCleaning..HousingProject

SELECT 
PARSENAME(REPLACE(OwnerAddress, ',','.'),3),
PARSENAME(REPLACE(OwnerAddress, ',','.'),2),
PARSENAME(REPLACE(OwnerAddress, ',','.'),1)
FROM SQL_DataCleaning..HousingProject

ALTER TABLE SQL_DataCleaning..HousingProject
ADD OwnerSplitAddress Nvarchar (255);
UPDATE SQL_DataCleaning..HousingProject
SET OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress, ',','.'),3) 

ALTER TABLE SQL_DataCleaning..HousingProject
ADD OwnerSplitCity Nvarchar (255);
UPDATE SQL_DataCleaning..HousingProject
SET OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress, ',','.'),2)

ALTER TABLE SQL_DataCleaning..HousingProject
ADD OwnerSplitState Nvarchar (255);
UPDATE SQL_DataCleaning..HousingProject
SET OwnerSplitState = PARSENAME(REPLACE(OwnerAddress, ',','.'),1)


----------------------------------------------------------------------------------------------------------------


--Change Y and N to Yes and No in "Sold as Vacant" field

SELECT DISTINCT(SoldAsVacant), COUNT(SoldAsVacant)
FROM SQL_DataCleaning..HousingProject
GROUP BY SoldAsVacant
ORDER BY 2


SELECT SoldAsVacant
, CASE WHEN SoldAsVacant = 'Y' THEN 'Yes'
	   WHEN SoldAsVacant = 'N' THEN 'No'
	   ELSE SoldAsVacant
	   END
FROM SQL_DataCleaning..HousingProject

UPDATE SQL_DataCleaning..HousingProject
SET SoldAsVacant = CASE WHEN SoldAsVacant = 'Y' THEN 'Yes'
	   WHEN SoldAsVacant = 'N' THEN 'No'
	   ELSE SoldAsVacant
	   END


----------------------------------------------------------------------------------------------------------------


-- Remove Duplicates (not a standard practice, done for demonstration purposes only)

WITH RowNumCTE AS(
SELECT *,
	ROW_NUMBER() OVER (
	PARTITION BY ParcelID,
				 PropertyAddress,
				 SalePrice,
				 SaleDate,
				 LegalReference
				 ORDER BY
					UniqueID
					) row_num

FROM SQL_DataCleaning..HousingProject )
DELETE
FROM RowNumCTE
WHERE row_num >1


----------------------------------------------------------------------------------------------------------------


-- Delete Unused Columns

ALTER TABLE SQL_DataCleaning..HousingProject
DROP COLUMN OwnerAddress, TaxDistrict, PropertyAddress, SaleDate


----------------------------------------------------------------------------------------------------------------


-- FINISH
