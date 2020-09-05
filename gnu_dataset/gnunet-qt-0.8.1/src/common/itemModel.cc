#include "itemModel.h"

GItemModel::GItemModel(QObject *parent)
{
  modelLock = new QMutex(QMutex::Recursive);
}

GItemModel::~GItemModel()
{
  delete modelLock;
}

bool GItemModel::hasIndex(int row, int column, const QModelIndex &parent) const
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::hasIndex(row, column, parent);
  modelLock->unlock();
  return ret;
}

QModelIndex GItemModel::index(int row, int column, const QModelIndex &parent) const
{
  QModelIndex ret;
  
  modelLock->lock();
  ret = QStandardItemModel::index(row, column, parent);
  modelLock->unlock();
  return ret;
}

QModelIndex GItemModel::parent(const QModelIndex &child) const
{
  QModelIndex ret;
  
  modelLock->lock();
  ret = QStandardItemModel::parent(child);
  modelLock->unlock();
  return ret;
}

void GItemModel::setColumnCount(int columns)
{
  modelLock->lock();
  QStandardItemModel::setColumnCount(columns);
  modelLock->unlock();
}

QStandardItem *GItemModel::itemFromIndex(const QModelIndex &index) const
{
  QStandardItem *ret;
  
  modelLock->lock();
  ret = QStandardItemModel::itemFromIndex(index);
  modelLock->unlock();
  return ret;
}

QStandardItem *GItemModel::invisibleRootItem() const
{
  QStandardItem *ret;
  
  modelLock->lock();
  ret = QStandardItemModel::invisibleRootItem();
  modelLock->unlock();  
  return ret;
}

QStandardItem *GItemModel::item(int row, int column) const
{
  QStandardItem *ret;
  
  modelLock->lock();
  ret = QStandardItemModel::item(row, column);
  modelLock->unlock();  
  return ret;
}

void GItemModel::clear()
{
  modelLock->lock();
  QStandardItemModel::clear();
  modelLock->unlock();
}

void GItemModel::appendRow(QStandardItem *item)
{
  modelLock->lock();
  QStandardItemModel::appendRow(item);
  modelLock->unlock();
}

int GItemModel::rowCount(const QModelIndex &parent) const
{
  int ret;
  
  modelLock->lock();
  ret = QStandardItemModel::rowCount(parent);
  modelLock->unlock();
  return ret;
}

int GItemModel::columnCount(const QModelIndex &parent) const
{
  int ret;
  
  modelLock->lock();
  ret = QStandardItemModel::columnCount(parent);
  modelLock->unlock();
  return ret;
}

bool GItemModel::hasChildren(const QModelIndex &parent) const
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::hasChildren(parent);
  modelLock->unlock();
  return ret;
}

QVariant GItemModel::data(const QModelIndex &index, int role) const
{
  QVariant ret;
  
  modelLock->lock();
  ret = QStandardItemModel::data(index, role);
  modelLock->unlock();
  return ret;
}

bool GItemModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::setData(index, value, role);
  modelLock->unlock();
  return ret;
}

QVariant GItemModel::headerData(int section, Qt::Orientation orientation, int role) const
{
  QVariant ret;
  
  modelLock->lock();
  ret = QStandardItemModel::headerData(section, orientation, role);
  modelLock->unlock();
  return ret;
}

bool GItemModel::setHeaderData(int section, Qt::Orientation orientation, const QVariant &value, int role)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::setHeaderData(section, orientation, value, role);
  modelLock->unlock();
  return ret;
}

QMap<int, QVariant> GItemModel::itemData(const QModelIndex &index) const
{
  QMap<int, QVariant> ret;
  
  modelLock->lock();
  ret = QStandardItemModel::itemData(index);
  modelLock->unlock();
  
  return ret;
}

bool GItemModel::setItemData(const QModelIndex &index, const QMap<int, QVariant> &roles)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::setItemData(index, roles);
  modelLock->unlock();
  return ret;
}

QStringList GItemModel::mimeTypes() const
{
  QStringList ret;
  
  modelLock->lock();
  ret = QStandardItemModel::mimeTypes();
  modelLock->unlock();
  
  return ret;
}

QMimeData *GItemModel::mimeData(const QModelIndexList &indexes) const
{
  QMimeData *ret;
  
  modelLock->lock();
  ret = QStandardItemModel::mimeData(indexes);
  modelLock->unlock();
  return ret;
}

bool GItemModel::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::dropMimeData(data, action, row, column, parent);
  modelLock->unlock();
  return ret;
}

Qt::DropActions GItemModel::supportedDropActions() const
{
  Qt::DropActions ret;
  
  modelLock->lock();
  ret = QStandardItemModel::supportedDropActions();
  modelLock->unlock();
  return ret;
}

Qt::DropActions GItemModel::supportedDragActions() const
{
  Qt::DropActions ret;
  
  modelLock->lock();
  ret = QStandardItemModel::supportedDragActions();
  modelLock->unlock();
  return ret;
}

void GItemModel::setSupportedDragActions(Qt::DropActions x)
{
  modelLock->lock();
  QStandardItemModel::setSupportedDragActions(x);
  modelLock->unlock();
}


bool GItemModel::insertRows(int row, int count, const QModelIndex &parent)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::insertRows(row, count, parent);
  modelLock->unlock();
  return ret;
}

bool GItemModel::insertColumns(int column, int count, const QModelIndex &parent)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::insertColumns(column, count, parent);
  modelLock->unlock();
  return ret;
}

bool GItemModel::removeRows(int row, int count, const QModelIndex &parent)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::removeRows(row, count, parent);
  modelLock->unlock();
  return ret;
}

bool GItemModel::removeColumns(int column, int count, const QModelIndex &parent)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::removeColumns(column, count, parent);
  modelLock->unlock();
  return ret;
}

inline bool GItemModel::insertRow(int row, const QModelIndex &parent)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::insertRow(row, parent);
  modelLock->unlock();
  
  return ret;
}

inline bool GItemModel::insertColumn(int column, const QModelIndex &parent)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::insertColumn(column, parent);
  modelLock->unlock();
  return ret;
}

bool GItemModel::removeRow(int row, const QModelIndex &parent)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::removeRow(row, parent);
  modelLock->unlock();
  
  return ret;
}

inline bool GItemModel::removeColumn(int column, const QModelIndex &parent)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::removeColumn(column, parent);
  modelLock->unlock();
  return ret;
}

void GItemModel::fetchMore(const QModelIndex &parent)
{
  modelLock->lock();
  QStandardItemModel::fetchMore(parent);
  modelLock->unlock();
}

bool GItemModel::canFetchMore(const QModelIndex &parent) const
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::canFetchMore(parent);
  modelLock->unlock();
  return ret;
}

Qt::ItemFlags GItemModel::flags(const QModelIndex &index) const
{
  Qt::ItemFlags ret;
  
  modelLock->lock();
  ret = QStandardItemModel::flags(index);
  modelLock->unlock();
  return ret;
}

void GItemModel::sort(int column, Qt::SortOrder order)
{
  modelLock->lock();
  QStandardItemModel::sort(column, order);
  modelLock->unlock();
}

QModelIndex GItemModel::buddy(const QModelIndex &index) const
{
  QModelIndex ret;
  
  modelLock->lock();
  ret = QStandardItemModel::buddy(index);
  modelLock->unlock();
  return ret;
}

QModelIndexList GItemModel::match(const QModelIndex &start, int role, const QVariant &value, int hits, Qt::MatchFlags flags) const
{
  QModelIndexList ret;
  
  modelLock->lock();
  ret = QStandardItemModel::match(start, role, value, hits, flags);
  modelLock->unlock();
  return ret;
}

QSize GItemModel::span(const QModelIndex &index) const
{
  QSize ret;
  
  modelLock->lock();
  ret = QStandardItemModel::span(index);
  modelLock->unlock();
  return ret;
}

inline QModelIndex GItemModel::createIndex(int row, int column, void *data) const
{
  QModelIndex ret;
  
  modelLock->lock();
  ret = QStandardItemModel::createIndex(row, column, data);
  modelLock->unlock();
  return ret;
}

inline QModelIndex GItemModel::createIndex(int row, int column, int id) const
{
  QModelIndex ret;
  
  modelLock->lock();
  ret = QStandardItemModel::createIndex(row, column, id);
  modelLock->unlock();
  return ret;
}

inline QModelIndex GItemModel::createIndex(int row, int column, quint32 id) const
{
  QModelIndex ret;
  
  modelLock->lock();
  ret = QStandardItemModel::createIndex(row, column, id);
  modelLock->unlock();
  return ret;
}

void GItemModel::encodeData(const QModelIndexList &indexes, QDataStream &stream) const
{
  modelLock->lock();
  QStandardItemModel::encodeData(indexes, stream);
  modelLock->unlock();
}

bool GItemModel::decodeData(int row, int column, const QModelIndex &parent, QDataStream &stream)
{
  bool ret;
  
  modelLock->lock();
  ret = QStandardItemModel::decodeData(row, column, parent, stream);
  modelLock->unlock();
  return ret;
}

void GItemModel::beginInsertRows(const QModelIndex &parent, int first, int last)
{
  modelLock->lock();
  QStandardItemModel::beginInsertRows(parent, first, last);
  modelLock->unlock();
}

void GItemModel::endInsertRows()
{
  modelLock->lock();
  QStandardItemModel::endInsertRows();
  modelLock->unlock();
}


void GItemModel::beginRemoveRows(const QModelIndex &parent, int first, int last)
{
  modelLock->lock();
  QStandardItemModel::beginRemoveRows(parent, first, last);
  modelLock->unlock();
}

void GItemModel::endRemoveRows()
{
  modelLock->lock();
  QStandardItemModel::endRemoveRows();
  modelLock->unlock();
}


void GItemModel::beginInsertColumns(const QModelIndex &parent, int first, int last)
{
  modelLock->lock();
  QStandardItemModel::beginInsertColumns(parent, first, last);
  modelLock->unlock();
}

void GItemModel::endInsertColumns()
{
  modelLock->lock();
  QStandardItemModel::endInsertColumns();
  modelLock->unlock();
}


void GItemModel::beginRemoveColumns(const QModelIndex &parent, int first, int last)
{
  modelLock->lock();
  QStandardItemModel::beginRemoveColumns(parent, first, last);
  modelLock->unlock();
}

void GItemModel::endRemoveColumns()
{
  modelLock->lock();
  QStandardItemModel::endRemoveColumns();
  modelLock->unlock();
}


void GItemModel::reset()
{
  modelLock->lock();
  QStandardItemModel::reset();
  modelLock->unlock();
}


void GItemModel::changePersistentIndex(const QModelIndex &from, const QModelIndex &to)
{
  modelLock->lock();
  QStandardItemModel::changePersistentIndex(from, to);
  modelLock->unlock();
}

void GItemModel::changePersistentIndexList(const QModelIndexList &from, const QModelIndexList &to)
{
  modelLock->lock();
  QStandardItemModel::changePersistentIndexList(from, to);
  modelLock->unlock();
}

QModelIndexList GItemModel::persistentIndexList() const
{
  QModelIndexList ret;
  
  modelLock->lock();
  ret = QStandardItemModel::persistentIndexList();
  modelLock->unlock();
  return ret;
}

void GItemModel::lock()
{
  modelLock->lock();
}

void GItemModel::unlock()
{
  modelLock->unlock();
}

QAbstractItemModel *GItemModel::abstractItemModel()
{
  return this;
}

QObject *GItemModel::object()
{
  return this;
}

